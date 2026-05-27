use egg::{Id, EGraph, Language, Extractor, FromOp, RecExpr, Rewrite, Subst, ENodeOrVar, PatternAst, CostFunction, Analysis, Runner, Report, StopReason, BackoffScheduler, SimpleScheduler, RewriteScheduler, SearchMatches, Iteration, IterationData};

use std::time::{Duration, Instant};

type RewriteId = usize;
type Cost = u128;

pub struct Limits {
    pub node_limit: usize,
    pub time_limit: Duration,
}

pub struct CostConfig<L> {
    pub cf: fn(&L) -> Cost,
    pub offset: Cost,
    pub unreachable_cost: Cost,
}

#[allow(unused)]
pub fn detour_run<L: Language, N: Analysis<L> + Default, IterData: IterationData<L, N>>(runner: Runner<L, N, IterData>, rws: &[Rewrite<L, N>], limits: Limits, cfg: CostConfig<L>) -> Runner<L, N, IterData> {
    let mut ctxt = Ctxt {
        runner,
        rws,
        limits,
        cfg,

        sched: BackoffScheduler::default(),
        start: Instant::now(),
    };

    // The initial e-graph might be dirty.
    ctxt.runner.egraph.rebuild();

    while ctxt.runner.stop_reason.is_none() {
        let mut body = || {
            ctxt.check_limits()?;

            call_hooks(&mut ctxt)?;

            ctxt.check_limits()?;

            detour_step(&mut ctxt)?;

            ctxt.check_limits()?;

            Ok(())
        };

        let it_start = Instant::now();
        let result = body();

        let it = mk_iteration(&mut ctxt, result.err(), it_start);
        ctxt.runner.iterations.push(it);
    }

    ctxt.runner
}

struct Ctxt<'a, L: Language, N: Analysis<L>, IterData: IterationData<L, N>> {
    runner: Runner<L, N, IterData>,
    rws: &'a [Rewrite<L, N>],
    limits: Limits,
    cfg: CostConfig<L>,
    sched: BackoffScheduler,

    start: Instant,
}

impl<'a, L: Language, N: Analysis<L>, IterData: IterationData<L, N>> Ctxt<'a, L, N, IterData> {
    fn check_limits(&self) -> Result<(), StopReason> {
        let elapsed = self.start.elapsed();
        if elapsed > self.limits.time_limit { return Err(StopReason::TimeLimit(elapsed.as_secs_f64())) }

        let size = self.runner.egraph.total_size();
        if size > self.limits.node_limit { return Err(StopReason::NodeLimit(size)) }

        Ok(())
    }
}

fn call_hooks<'a, L: Language, N: Analysis<L>, IterData: IterationData<L, N>>(ctxt: &mut Ctxt<'a, L, N, IterData>) -> Result<(), StopReason> {
    let mut hooks = std::mem::take(&mut ctxt.runner.hooks);
    let res = hooks.iter_mut().try_for_each(|hook| hook(&mut ctxt.runner).map_err(StopReason::Other));
    ctxt.runner.hooks = hooks;
    res
}

fn detour_step<'a, L: Language, N: Analysis<L>, IterData: IterationData<L, N>>(ctxt: &mut Ctxt<'a, L, N, IterData>) -> Result<(), StopReason> {
    let i = ctxt.runner.iterations.len();
    if i % 2 == 1 {
        let mut matches = Vec::new();

        // here we don't use the normal scheduler, but instead we use our own!
        let mut sched = BackoffScheduler::default();

        for rw in ctxt.rws {
            matches.push(sched.search_rewrite(i, &ctxt.runner.egraph, rw));
            ctxt.check_limits()?;
        }

        for (rw, ms) in ctxt.rws.iter().zip(matches.into_iter()) {
            sched.apply_rewrite(i, &mut ctxt.runner.egraph, rw, ms);
            ctxt.check_limits()?;
        }

        ctxt.runner.egraph.rebuild();

        return Ok(());
    }

    pat_detour_eqsat_step(ctxt)
}

fn pat_detour_eqsat_step<'a, L: Language, N: Analysis<L>, IterData: IterationData<L, N>>(ctxt: &mut Ctxt<'a, L, N, IterData>) -> Result<(), StopReason> {
    let ex = Extractor::new(&ctxt.runner.egraph, AdditiveCostFn(ctxt.cfg.cf));
    let ctxt_cost = compute_ctxt_costs(&ex, ctxt);

    let mut matches: BTreeMap</*detour cost*/ Cost, Vec<(RewriteId, Id, Subst)>> = BTreeMap::default();

    for (rw_i, rw) in ctxt.rws.iter().enumerate() {
        let lhs_pat = rw.searcher.get_pattern_ast().unwrap();

        for m in ctxt.sched.search_rewrite(ctxt.runner.iterations.len(), &ctxt.runner.egraph, rw) {
            ctxt.check_limits()?;

            let lhs = m.eclass;
            for subst in m.substs {
                let pat_cost = pat_cost(lhs_pat, &subst, &ex, ctxt.cfg.cf);
                let cx_cost = *ctxt_cost.get(&lhs).unwrap_or(&ctxt.cfg.unreachable_cost); // this is the cost you get from not being able to reach any root.
                let detour_cost = cx_cost + pat_cost;
                matches.entry(detour_cost).or_insert(Vec::new()).push((rw_i, lhs, subst));

                ctxt.check_limits()?;
            }
        }
    }

    let eg_data = |eg: &EGraph<_, _>| (eg.number_of_classes(), eg.total_size());

    let og_data = eg_data(&ctxt.runner.egraph);
    let mut found_cost = None;

    for (full_cost, new_apps) in matches {
        if let Some(found) = found_cost { if full_cost > found + ctxt.cfg.offset { break } }
        for (rw_i, lhs, subst) in &new_apps {
            let rw = &ctxt.rws[*rw_i];
            let pat_ast = rw.searcher.get_pattern_ast();
            rw.applier.apply_one(&mut ctxt.runner.egraph, *lhs, subst, pat_ast, rw.name);
            if eg_data(&ctxt.runner.egraph) != og_data && found_cost.is_none() { found_cost = Some(full_cost); }

            ctxt.check_limits()?;
        }
    }

    ctxt.runner.egraph.rebuild();
    Ok(())
}

// === ctxt cost ===

fn compute_ctxt_costs<'a, L: Language, N: Analysis<L>, IterData: IterationData<L, N>>(ex: &Extractor<AdditiveCostFn<L>, L, N>, ctxt: &Ctxt<'a, L, N, IterData>) -> HashMap<Id, Cost> {
    let mut ctxt_cost = HashMap::new();

    let mut queue: MinPrioQueue<Cost, Id> = MinPrioQueue::new();

    // initial
    for root in &ctxt.runner.roots {
        queue.push(0, *root);
    }

    while let Some((cst, i)) = queue.pop() {
        if ctxt_cost.contains_key(&i) { continue }
        ctxt_cost.insert(i, cst);
        for e in &ctxt.runner.egraph[i].nodes {
            let e_cost = AdditiveCostFn(ctxt.cfg.cf).cost(e, |k| ex.find_best_cost(k));
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

fn mk_iteration<'a, L: Language, N: Analysis<L> + Default, IterData: IterationData<L, N>>(ctxt: &mut Ctxt<'a, L, N, IterData>, stop_reason: Option<StopReason>, it_start: Instant) -> Iteration<IterData> {
    let eg = std::mem::take(&mut ctxt.runner.egraph);
    let mut mock_runner = Runner::new(N::default()).with_egraph(eg);
    mock_runner.roots = ctxt.runner.roots.clone();
    mock_runner = mock_runner.run([]);
    let mut it = mock_runner.iterations.pop().unwrap();
    ctxt.runner.egraph = mock_runner.egraph;

    it.egraph_nodes = it.egraph_nodes; // set correctly by mock runner
    it.egraph_classes = it.egraph_classes; // set correctly by mock runner
    it.applied = Default::default(); // set to default
    it.hook_time = 0.0; // set to default
    it.search_time = 0.0; // set to default
    it.apply_time = 0.0; // set to default
    it.rebuild_time = 0.0; // set to default
    it.total_time = it_start.elapsed().as_secs_f64(); // Note that this iteration counting counts *everything* in an iteration. This is different from egg, which excludes hooks etc.
    it.data = it.data; // set correctly by mock runner
    it.n_rebuilds = 0; // set to default
    it.stop_reason = stop_reason.clone();

    if stop_reason.is_some() {
        ctxt.runner.stop_reason = stop_reason;
    }

    it
}
