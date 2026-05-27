use egg::{Id, EGraph, Language, Extractor, FromOp, RecExpr, Rewrite, Subst, ENodeOrVar, PatternAst, CostFunction, Analysis, Runner, Report, StopReason, BackoffScheduler, SimpleScheduler, RewriteScheduler, SearchMatches};

use std::fmt::Display;
use std::time::{Duration, Instant};

pub type Hook<L, N> = Box<dyn FnMut(&mut EGraph<L, N>) -> Result<(), String>>;
type RewriteId = usize;
type Cost = u128;

struct Ctxt<'a, L: Language, N: Analysis<L>> {
    roots: &'a [Id],
    rws: &'a [Rewrite<L, N>],
    eg: &'a mut EGraph<L, N>,
    hooks: &'a mut [Hook<L, N>],
    cf: fn(&L) -> Cost, // note: 'cf: fn(&L) -> Cost' will ignore the costs of the children!
    cfg_offset: Cost,
    cfg_unreachable_cost: Cost,

    // internal state:
    sched: BackoffScheduler,
    stop_reason: StopReason,
    stopper: Stopper,
    i: usize,
}

pub fn detour_run<L: Language, N: Analysis<L> + Default>(roots: &[Id], rws: &[Rewrite<L, N>], eg: &mut EGraph<L, N>, hooks: &mut [Hook<L, N>], time_limit: Duration, node_limit: usize, cf: fn(&L) -> Cost, cfg_offset: Cost, cfg_unreachable_cost: Cost) -> Report {
    run(&mut Ctxt {
        roots,
        rws,
        eg,
        hooks,
        cf,
        cfg_offset,
        cfg_unreachable_cost,

        sched: BackoffScheduler::default(),

        stop_reason: StopReason::Saturated,

        stopper: Stopper {
            start: Instant::now(),
            time_limit,
            node_limit
        },
        i: 0,
    })
}

fn run<'a, L: Language, N: Analysis<L>>(ctxt: &mut Ctxt<'a, L, N>) -> Report {
    // The initial e-graph might be dirty.
    ctxt.eg.rebuild();

    loop {
        let mut body = || {
            ctxt.stopper.check_limits(ctxt.eg)?;

            for h in ctxt.hooks.iter_mut() {
                h(ctxt.eg).map_err(StopReason::Other)?;
            }

            ctxt.stopper.check_limits(ctxt.eg)?;

            detour_step(ctxt)?;

            ctxt.stopper.check_limits(ctxt.eg)?;

            ctxt.i += 1;

            Ok(())
        };

        if let Err(sr) = body() { ctxt.stop_reason = sr; break }
    }

    let total_time = ctxt.stopper.start.elapsed().as_secs_f64();

    let mut report = Runner::<L, ()>::new(()).run(&[]).report();

    report.stop_reason = ctxt.stop_reason.clone();
    report.total_time = total_time;

    report.iterations = ctxt.i+1; // in egg, any aborted iteration still counts as an iteration and increments the counter.
    report.egraph_nodes = ctxt.eg.total_number_of_nodes();
    report.egraph_classes = ctxt.eg.number_of_classes();
    report.memo_size = ctxt.eg.total_size();

    // unknown
    report.rebuilds = 0;
    report.search_time = 0.0;
    report.apply_time = 0.0;
    report.rebuild_time = 0.0;

    report
}

fn detour_step<'a, L: Language, N: Analysis<L>>(ctxt: &mut Ctxt<'a, L, N>) -> Result<(), StopReason> {
    if ctxt.i%2 == 1 {
        let mut matches = Vec::new();

        // here we don't use the normal scheduler, but instead we use our own!
        let mut sched = BackoffScheduler::default();

        for rw in ctxt.rws {
            matches.push(sched.search_rewrite(ctxt.i, ctxt.eg, rw));
            ctxt.stopper.check_limits(ctxt.eg)?;
        }

        for (rw, ms) in ctxt.rws.iter().zip(matches.into_iter()) {
            sched.apply_rewrite(ctxt.i, ctxt.eg, rw, ms);
            ctxt.stopper.check_limits(ctxt.eg)?;
        }

        ctxt.eg.rebuild();

        return Ok(());
    }

    pat_detour_eqsat_step(ctxt)
}

fn pat_detour_eqsat_step<'a, L: Language, N: Analysis<L>>(ctxt: &mut Ctxt<'a, L, N>) -> Result<(), StopReason> {
    let ex = Extractor::new(&ctxt.eg, AdditiveCostFn(ctxt.cf));
    let ctxt_cost = compute_ctxt_costs(&ex, ctxt);

    let mut matches: BTreeMap</*detour cost*/ Cost, Vec<(RewriteId, Id, Subst)>> = BTreeMap::default();

    for (rw_i, rw) in ctxt.rws.iter().enumerate() {
        let lhs_pat = rw.searcher.get_pattern_ast().unwrap();

        for m in ctxt.sched.search_rewrite(ctxt.i, ctxt.eg, rw) {
            ctxt.stopper.check_limits(ctxt.eg)?;

            let lhs = m.eclass;
            for subst in m.substs {
                let pat_cost = pat_cost(lhs_pat, &subst, &ex, ctxt.cf);
                let cx_cost = *ctxt_cost.get(&lhs).unwrap_or(&ctxt.cfg_unreachable_cost); // this is the cost you get from not being able to reach any root.
                let detour_cost = cx_cost + pat_cost;
                matches.entry(detour_cost).or_insert(Vec::new()).push((rw_i, lhs, subst));

                ctxt.stopper.check_limits(ctxt.eg)?;
            }
        }
    }

    let eg_data = |eg: &EGraph<_, _>| (eg.number_of_classes(), eg.total_size());

    let og_data = eg_data(ctxt.eg);
    let mut found_cost = None;

    'outer: for (full_cost, new_apps) in matches {
        if let Some(found) = found_cost { if full_cost > found + ctxt.cfg_offset { break } }
        for (rw_i, lhs, subst) in &new_apps {
            let rw = &ctxt.rws[*rw_i];
            let pat_ast = rw.searcher.get_pattern_ast();
            rw.applier.apply_one(ctxt.eg, *lhs, subst, pat_ast, rw.name);
            if eg_data(ctxt.eg) != og_data && found_cost.is_none() { found_cost = Some(full_cost); }

            ctxt.stopper.check_limits(ctxt.eg)?;
        }
    }

    ctxt.eg.rebuild();
    Ok(())
}

// === ctxt cost ===

fn compute_ctxt_costs<'a, L: Language, N: Analysis<L>>(ex: &Extractor<AdditiveCostFn<L>, L, N>, ctxt: &Ctxt<'a, L, N>) -> HashMap<Id, Cost> {
    let mut ctxt_cost = HashMap::new();

    let mut queue: MinPrioQueue<Cost, Id> = MinPrioQueue::new();

    // initial
    for root in ctxt.roots {
        queue.push(0, *root);
    }

    while let Some((cst, i)) = queue.pop() {
        if ctxt_cost.contains_key(&i) { continue }
        ctxt_cost.insert(i, cst);
        for e in &ctxt.eg[i].nodes {
            let e_cost = AdditiveCostFn(ctxt.cf).cost(e, |k| ex.find_best_cost(k));
            for &c in e.children() {
                // optimization: don't push junk to the queue.
                // NOTE: if we remembered what's the best thing we already pushed to the queue for some class,
                // we could do more efficient pruning.
                if ctxt_cost.contains_key(&c) { continue }

                let c_cost = ex.find_best_cost(c);
                let ncst = e_cost + cst - c_cost;
                queue.push(ncst, c);
            }
        }
    }

    ctxt_cost
}

fn pat_cost<L: Language, N: Analysis<L>>(pat: &PatternAst<L>, subst: &Subst, ex: &Extractor<AdditiveCostFn<L>, L, N>, cf: fn(&L) -> Cost) -> Cost {
    let mut vec: Vec<Cost> = Vec::new();
    for i in 0..pat.as_ref().len() {
        let cost = match &pat[i.into()] {
            ENodeOrVar::ENode(n) => AdditiveCostFn(cf).cost(n, |x| vec[usize::from(x)]),
            ENodeOrVar::Var(v) => ex.find_best_cost(subst[*v]),
        };
        vec.push(cost);
    }
    vec.last().copied().unwrap()
}

// === misc ===

fn lookup_pat<L: Language, N: Analysis<L>>(pat: &PatternAst<L>, eg: &EGraph<L, N>, subst: &Subst) -> Option<Id> {
    let mut vec = Vec::new();
    for i in 0..pat.as_ref().len() {
        match &pat[i.into()] {
            ENodeOrVar::ENode(n) => {
                let mut n = n.clone().map_children(|k| vec[usize::from(k)]);
                let k = eg.lookup(&mut n)?;
                vec.push(k);
            },
            ENodeOrVar::Var(v) => vec.push(subst[*v]),
        }
    }
    vec.last().copied()
}


// === minqueue ===

use std::cmp::Ordering;
use std::collections::{BinaryHeap, HashMap, BTreeMap};

struct MinPrioQueue<U, T>(BinaryHeap<WithOrdRev<U, T>>);

impl<U: Ord, T: Eq> MinPrioQueue<U, T> {
    pub fn new() -> Self {
        MinPrioQueue(BinaryHeap::default())
    }

    pub fn push(&mut self, u: U, t: T) {
        self.0.push(WithOrdRev(u, t));
    }

    pub fn pop(&mut self) -> Option<(U, T)> {
        self.0.pop().map(|WithOrdRev(u, t)| (u, t))
    }
}

// Takes the `Ord` from U, but reverses it.
#[derive(PartialEq, Eq, Debug)]
struct WithOrdRev<U, T>(pub U, pub T);

impl<U: Ord, T: Eq> PartialOrd for WithOrdRev<U, T> {
    fn partial_cmp(&self, other: &Self) -> Option<Ordering> {
        // It's the other way around, because we want a min-heap!
        other.0.partial_cmp(&self.0)
    }
}
impl<U: Ord, T: Eq> Ord for WithOrdRev<U, T> {
    fn cmp(&self, other: &Self) -> Ordering {
        self.partial_cmp(&other).unwrap()
    }
}

// === AdditiveCostFn ===

struct AdditiveCostFn<L: Language>(fn(&L) -> Cost);

impl<L: Language> CostFunction<L> for AdditiveCostFn<L> {
    type Cost = Cost;

    fn cost<C>(&mut self, enode: &L, costs: C) -> Self::Cost where C: FnMut(Id) -> Self::Cost {
        enode.children().iter().copied().map(costs).fold(self.0(enode), |x, y| x+y)
    }
}

// === Stopper ===

struct Stopper {
    start: Instant,
    time_limit: Duration,
    node_limit: usize,
}

impl Stopper {
    fn check_limits<L: Language, N: Analysis<L>>(&self, eg: &EGraph<L, N>) -> Result<(), StopReason> {
        let elapsed = self.start.elapsed();
        if elapsed > self.time_limit { return Err(StopReason::TimeLimit(elapsed.as_secs_f64())) }

        let size = eg.total_size();
        if size > self.node_limit { return Err(StopReason::NodeLimit(size)) }

        Ok(())
    }
}
