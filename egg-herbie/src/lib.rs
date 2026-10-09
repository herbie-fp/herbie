#![allow(clippy::missing_safety_doc)]

pub mod math;

use egg::{
    BackoffScheduler, Extractor, FromOp, Id, Language, RewriteScheduler, SearchMatches,
    SimpleScheduler, StopReason,
};
use libc::{c_void, strlen};
use math::*;

use std::cell::RefCell;
use std::cmp::min;
use std::collections::HashMap;
use std::ffi::{CStr, CString};
use std::mem::{self, ManuallyDrop};
use std::os::raw::c_char;
use std::rc::Rc;
use std::time::{Duration, Instant};
use std::{slice, sync::atomic::Ordering};

pub struct Context {
    runner: Runner,
    rules: Vec<Rewrite>,
    best: Option<HashMap<u32, (usize, Math)>>,
}

struct TimingScheduler<S> {
    inner: S,
    search_times: Rc<RefCell<HashMap<String, (f64, usize, usize, usize)>>>,
}

impl<S> RewriteScheduler<Math, ConstantFold> for TimingScheduler<S>
where
    S: RewriteScheduler<Math, ConstantFold>,
{
    fn can_stop(&mut self, iteration: usize) -> bool {
        self.inner.can_stop(iteration)
    }

    fn search_rewrite<'a>(
        &mut self,
        iteration: usize,
        egraph: &EGraph,
        rewrite: &'a Rewrite,
    ) -> Vec<SearchMatches<'a, Math>> {
        let started = Instant::now();
        let matches = self.inner.search_rewrite(iteration, egraph, rewrite);
        let mut search_times = self.search_times.borrow_mut();
        let stats = search_times.entry(rewrite.name.to_string()).or_default();
        stats.0 += started.elapsed().as_secs_f64() * 1000.0;
        stats.1 += 1;
        stats.2 += matches.len();
        stats.3 += matches.iter().map(|matched| matched.substs.len()).sum::<usize>();
        matches
    }

    fn apply_rewrite(
        &mut self,
        iteration: usize,
        egraph: &mut EGraph,
        rewrite: &Rewrite,
        matches: Vec<SearchMatches<Math>>,
    ) -> usize {
        self.inner
            .apply_rewrite(iteration, egraph, rewrite, matches)
    }
}

// I had to add $(rustc --print sysroot)/lib to LD_LIBRARY_PATH to get linking to work after installing rust with rustup
#[no_mangle]
pub unsafe extern "C" fn egraph_create() -> *mut Context {
    Box::into_raw(Box::new(Context {
        runner: Runner::new(Default::default()).with_explanations_enabled(),
        rules: vec![],
        best: None,
    }))
}

#[no_mangle]
pub unsafe extern "C" fn egraph_destroy(ptr: *mut Context) {
    drop(Box::from_raw(ptr))
}

#[no_mangle]
pub unsafe extern "C" fn destroy_egraphiters(ptr: *mut c_void) {
    // TODO: Switch ffi to use `usize` directly to avoid the risk of these being incorrect
    drop(Box::from_raw(ptr as *mut Vec<EGraphIter>));
    // drop(Vec::from_raw_parts(data, length as usize, capacity as usize))
}

#[no_mangle]
pub unsafe extern "C" fn destroy_string(ptr: *mut c_char) {
    drop(CString::from_raw(ptr))
}

#[no_mangle]
pub unsafe extern "C" fn string_length(ptr: *const c_char) -> u32 {
    strlen(ptr) as u32
}

#[repr(C)]
pub struct EGraphIter {
    numnodes: u32,
    numclasses: u32,
    time: f64,
}

// a struct for loading rules from external source
#[repr(C)]
pub struct FFIRule {
    name: *const c_char,
    left: *const c_char,
    right: *const c_char,
}

#[no_mangle]
pub unsafe extern "C" fn egraph_add_root(ptr: *mut Context, id: u32) {
    let mut context = ManuallyDrop::new(Box::from_raw(ptr));
    context.runner.roots.push(Id::from(id as usize));
}

#[no_mangle]
pub unsafe extern "C" fn egraph_add_node(
    ptr: *mut Context,
    f: *const c_char,
    ids_ptr: *const u32,
    num_ids: u32,
) -> u32 {
    let _ = env_logger::try_init();
    // Safety: `ptr` was box allocated by `egraph_create`
    let mut context = ManuallyDrop::new(Box::from_raw(ptr));
    context.best = None;

    let f = CStr::from_ptr(f).to_str().unwrap();
    let len = num_ids as usize;
    let ids: &[u32] = slice::from_raw_parts(ids_ptr, len);
    let ids = ids.iter().map(|id| Id::from(*id as usize)).collect();
    let node = Math::from_op(f, ids).unwrap();
    let id = context.runner.egraph.add(node);
    usize::from(id) as u32
}

#[no_mangle]
pub unsafe extern "C" fn egraph_seed_do_lower(
    ptr: *mut Context,
    f: *const c_char,
    ids_ptr: *const u32,
    num_ids: u32,
    output_ptr: *mut u32,
) {
    let timing = timing_enabled();
    let started = timing.then(Instant::now);
    let f = CStr::from_ptr(f).to_str().unwrap();
    let ids = slice::from_raw_parts(ids_ptr, num_ids as usize);
    let mut context = ManuallyDrop::new(Box::from_raw(ptr));
    context.best = None;
    let nodes_before = timing.then(|| context.runner.egraph.total_size());
    for (i, id) in ids.iter().enumerate() {
        let spec_id = Id::from(*id as usize);
        let do_lower_id = context
            .runner
            .egraph
            .add(Math::from_op(f, vec![spec_id]).unwrap());
        std::ptr::write(output_ptr.offset(i as isize), usize::from(do_lower_id) as u32);
    }
    if let (Some(started), Some(nodes_before)) = (started, nodes_before) {
        eprintln!(
            "EGG_TIMING seed op={} ids={} nodes_before={} nodes_after={} elapsed_ms={:.3}",
            f,
            ids.len(),
            nodes_before,
            context.runner.egraph.total_size(),
            started.elapsed().as_secs_f64() * 1000.0,
        );
    }
}

#[no_mangle]
pub unsafe extern "C" fn egraph_add_node_to_eclass(
    ptr: *mut Context,
    class_id: u32,
    f: *const c_char,
    ids_ptr: *const u32,
    num_ids: u32,
) {
    let f = CStr::from_ptr(f).to_str().unwrap();
    let ids = slice::from_raw_parts(ids_ptr, num_ids as usize)
        .iter()
        .map(|id| Id::from(*id as usize))
        .collect();
    let mut context = ManuallyDrop::new(Box::from_raw(ptr));
    context.best = None;
    let node_id = context
        .runner
        .egraph
        .add(Math::from_op(f, ids).unwrap());
    context
        .runner
        .egraph
        .union(Id::from(class_id as usize), node_id);
}

#[no_mangle]
pub unsafe extern "C" fn egraph_add_node_to_eclass_with_reason(
    ptr: *mut Context,
    class_id: u32,
    f: *const c_char,
    ids_ptr: *const u32,
    num_ids: u32,
    reason: *const c_char,
) {
    let f = CStr::from_ptr(f).to_str().unwrap();
    let reason = CStr::from_ptr(reason).to_str().unwrap().to_owned();
    let ids = slice::from_raw_parts(ids_ptr, num_ids as usize)
        .iter()
        .map(|id| Id::from(*id as usize))
        .collect();
    let mut context = ManuallyDrop::new(Box::from_raw(ptr));
    context.best = None;
    let node_id = context
        .runner
        .egraph
        .add(Math::from_op(f, ids).unwrap());
    context
        .runner
        .egraph
        .union_trusted(Id::from(class_id as usize), node_id, reason);
}

#[no_mangle]
pub unsafe extern "C" fn egraph_copy(ptr: *mut Context) -> *mut Context {
    // Safety: `ptr` was box allocated by `egraph_create`
    let context = Box::from_raw(ptr);
    let mut runner = Runner::new(Default::default())
        .with_explanations_enabled()
        .with_egraph(context.runner.egraph.clone());
    runner.roots = context.runner.roots.clone();
    runner.egraph.rebuild();

    mem::forget(context);

    Box::into_raw(Box::new(Context {
        rules: vec![],
        runner,
        best: None,
    }))
}

unsafe fn ptr_to_string(ptr: *const c_char) -> String {
    let bytes = CStr::from_ptr(ptr).to_bytes();
    String::from_utf8(bytes.to_vec()).unwrap()
}

// todo don't just unwrap, also make sure the rules are validly parsed
unsafe fn ffirule_to_tuple(rule_ptr: *mut FFIRule) -> (String, String, String) {
    let rule = &mut *rule_ptr;
    (
        ptr_to_string(rule.name),
        ptr_to_string(rule.left),
        ptr_to_string(rule.right),
    )
}

#[no_mangle]
pub unsafe extern "C" fn egraph_run(
    ptr: *mut Context,
    rules_array_ptr: *const *mut FFIRule,
    rules_array_length: u32,
    iterations_length: *mut u32,
    iterations_ptr: *mut *mut c_void,
    iter_limit: u32,
    node_limit: u32,
    simple_scheduler: bool,
) -> *const EGraphIter {
    let timing = timing_enabled();
    let ffi_started = timing.then(Instant::now);
    // Safety: `ptr` was box allocated by `egraph_create`
    let mut context = Box::from_raw(ptr);
    let nodes_before = timing.then(|| context.runner.egraph.total_size());
    let classes_before = timing.then(|| context.runner.egraph.number_of_classes());
    let mut parse_ms = 0.0;
    let mut runner_ms = 0.0;
    let mut rule_count = 0;
    let mut lower_rule_count = 0;
    let mut search_times = None;

    if context.runner.stop_reason.is_none() {
        let parse_started = timing.then(Instant::now);
        let length: usize = rules_array_length as usize;
        let ffi_rules: &[*mut FFIRule] = slice::from_raw_parts(rules_array_ptr, length);
        let mut ffi_tuples: Vec<(&str, &str, &str)> = vec![];
        let mut ffi_strings: Vec<(String, String, String)> = vec![];
        for ffi_rule in ffi_rules.iter() {
            let str_tuple = ffirule_to_tuple(*ffi_rule);
            ffi_strings.push(str_tuple);
        }

        for ffi_string in ffi_strings.iter() {
            ffi_tuples.push((&ffi_string.0, &ffi_string.1, &ffi_string.2));
        }

        rule_count = ffi_strings.len();
        lower_rule_count = ffi_strings
            .iter()
            .filter(|(name, _, _)| name.contains("lower"))
            .count();
        let rules: Vec<Rewrite> = math::mk_rules(&ffi_tuples);
        parse_ms = parse_started
            .map_or(0.0, |started| started.elapsed().as_secs_f64() * 1000.0);
        context.rules = rules;

        let search_times_for_scheduler =
            timing.then(|| Rc::new(RefCell::new(HashMap::new())));
        search_times = search_times_for_scheduler.clone();
        let runner_started = timing.then(Instant::now);
        context.runner = match (timing, simple_scheduler) {
            (true, true) => context.runner.with_scheduler(TimingScheduler {
                inner: SimpleScheduler,
                search_times: Rc::clone(search_times_for_scheduler.as_ref().unwrap()),
            }),
            (true, false) => context.runner.with_scheduler(TimingScheduler {
                inner: BackoffScheduler::default(),
                search_times: Rc::clone(search_times_for_scheduler.as_ref().unwrap()),
            }),
            (false, true) => context.runner.with_scheduler(SimpleScheduler),
            (false, false) => {
                context.runner.with_scheduler(BackoffScheduler::default())
            }
        };

        context.runner = context
            .runner
            .with_node_limit(node_limit as usize)
            .with_iter_limit(iter_limit as usize) // should never hit
            .with_time_limit(Duration::from_secs(u64::MAX))
            .with_hook(|r| {
                if r.egraph.analysis.unsound.load(Ordering::SeqCst) {
                    Err("Unsoundness detected".into())
                } else {
                    Ok(())
                }
            })
            .run(&context.rules);
        runner_ms = runner_started
            .map_or(0.0, |started| started.elapsed().as_secs_f64() * 1000.0);
    }

    context.best = None;

    // Prune all e-nodes with children where its e-class has a leaf node (with no children). Pruning
    // safely improves performance because pruning occurs right before extraction and leaf e-nodes
    // always have a lower cost.
    let nodes_after_run = timing.then(|| context.runner.egraph.total_size());
    let classes_after_run = timing.then(|| context.runner.egraph.number_of_classes());
    let prune_started = timing.then(Instant::now);
    context.runner.egraph.classes_mut().for_each(|eclass| {
        if eclass.nodes.iter().any(|n| n.is_leaf()) {
            eclass.nodes.retain(|n| n.is_leaf());
        }
    });
    let prune_ms = prune_started
        .map_or(0.0, |started| started.elapsed().as_secs_f64() * 1000.0);

    if let (
        Some(ffi_started),
        Some(nodes_before),
        Some(classes_before),
        Some(nodes_after_run),
        Some(classes_after_run),
    ) = (
        ffi_started,
        nodes_before,
        classes_before,
        nodes_after_run,
        classes_after_run,
    ) {
        let iterations = &context.runner.iterations;
        let search_ms: f64 = iterations.iter().map(|it| it.search_time * 1000.0).sum();
        let apply_ms: f64 = iterations.iter().map(|it| it.apply_time * 1000.0).sum();
        let rebuild_ms: f64 = iterations.iter().map(|it| it.rebuild_time * 1000.0).sum();
        let iteration_ms: f64 = iterations.iter().map(|it| it.total_time * 1000.0).sum();
        let iteration_other_ms = iteration_ms - search_ms - apply_ms - rebuild_ms;
        let mut applied_by_rule = HashMap::<String, usize>::new();
        for iteration in iterations {
            for (name, count) in &iteration.applied {
                *applied_by_rule.entry(name.to_string()).or_default() += count;
            }
        }
        let mut applied_by_rule: Vec<_> = applied_by_rule.into_iter().collect();
        applied_by_rule.sort_by_key(|(_, count)| std::cmp::Reverse(*count));
        applied_by_rule.truncate(5);
        let mut top_search_times = search_times
            .as_ref()
            .map(|search_times| {
                search_times
                    .borrow()
                    .iter()
                    .map(|(name, (time, calls, _, _))| (name.clone(), *time, *calls))
                    .collect::<Vec<_>>()
            })
            .unwrap_or_default();
        top_search_times.sort_by(|a, b| b.1.total_cmp(&a.1));
        top_search_times.truncate(10);
        let mut search_by_family = HashMap::<&str, (usize, usize, usize, usize, f64)>::new();
        if let Some(search_times) = &search_times {
            for (name, (time, calls, matched_classes, substitutions)) in
                search_times.borrow().iter()
            {
                let family = if name.starts_with("lower-constant-repr-") {
                    "constant_repr"
                } else if name.starts_with("lower-variable-repr-") {
                    "variable_repr"
                } else if name.starts_with("do-lower-") {
                    "operator_lower"
                } else {
                    "other"
                };
                let stats = search_by_family.entry(family).or_default();
                stats.0 += 1;
                stats.1 += calls;
                stats.2 += matched_classes;
                stats.3 += substitutions;
                stats.4 += time;
            }
        }
        let mut search_by_family: Vec<_> = search_by_family.into_iter().collect();
        search_by_family.sort_by(|a, b| b.1 .4.total_cmp(&a.1 .4));
        eprintln!(
            "EGG_TIMING run scheduler={} rules={} lower_rules={} nodes_before={} classes_before={} nodes_after_run={} classes_after_run={} nodes_after_prune={} iterations={} parse_ms={:.3} runner_wall_ms={:.3} runner_iteration_ms={:.3} search_ms={:.3} apply_ms={:.3} rebuild_ms={:.3} iteration_other_ms={:.3} prune_ms={:.3} total_ms={:.3} search_by_family={:?} top_search_ms={:?} top_applied={:?} stop={:?}",
            if simple_scheduler { "simple" } else { "backoff" },
            rule_count,
            lower_rule_count,
            nodes_before,
            classes_before,
            nodes_after_run,
            classes_after_run,
            context.runner.egraph.total_size(),
            iterations.len(),
            parse_ms,
            runner_ms,
            iteration_ms,
            search_ms,
            apply_ms,
            rebuild_ms,
            iteration_other_ms,
            prune_ms,
            ffi_started.elapsed().as_secs_f64() * 1000.0,
            search_by_family,
            top_search_times,
            applied_by_rule,
            context.runner.stop_reason,
        );
    }

    let iterations = context
        .runner
        .iterations
        .iter()
        .map(|iteration| EGraphIter {
            numnodes: iteration.egraph_nodes as u32,
            numclasses: iteration.egraph_classes as u32,
            time: iteration.total_time,
        })
        .collect::<Vec<_>>();
    let iterations_data = iterations.as_ptr();

    std::ptr::write(iterations_length, iterations.len() as u32);
    std::ptr::write(
        iterations_ptr,
        Box::into_raw(Box::new(iterations)) as *mut c_void,
    );
    mem::forget(context);

    iterations_data
}

#[no_mangle]
pub unsafe extern "C" fn egraph_get_stop_reason(ptr: *mut Context) -> u32 {
    // Safety: `ptr` was box allocated by `egraph_create`
    let context = ManuallyDrop::new(Box::from_raw(ptr));

    match context.runner.stop_reason {
        Some(StopReason::Saturated) => 0,
        Some(StopReason::IterationLimit(_)) => 1,
        Some(StopReason::NodeLimit(_)) => 2,
        Some(StopReason::Other(_)) => 3,
        _ => 4,
    }
}

fn find_extracted(runner: &Runner, id: u32, iter: u32) -> &Extracted {
    let id = runner.egraph.find(Id::from(id as usize));

    // go back one more iter, egg can duplicate the final iter in the case of an error
    let is_unsound = runner.egraph.analysis.unsound.load(Ordering::SeqCst);
    let sound_iter = min(
        runner
            .iterations
            .len()
            .saturating_sub(if is_unsound { 3 } else { 1 }),
        iter as usize,
    );

    runner.iterations[sound_iter]
        .data
        .extracted
        .iter()
        .find(|(i, _)| runner.egraph.find(*i) == id)
        .map(|(_, ext)| ext)
        .expect("Couldn't find matching extraction!")
}

#[no_mangle]
pub unsafe extern "C" fn egraph_find(ptr: *mut Context, id: usize) -> u32 {
    let context = ManuallyDrop::new(Box::from_raw(ptr));
    let node_id = Id::from(id);
    let canon_id = context.runner.egraph.find(node_id);
    usize::from(canon_id) as u32
}

#[no_mangle]
pub unsafe extern "C" fn egraph_size(ptr: *mut Context) -> u32 {
    let context = ManuallyDrop::new(Box::from_raw(ptr));
    context.runner.egraph.number_of_classes() as u32
}

#[no_mangle]
pub unsafe extern "C" fn egraph_eclass_size(ptr: *mut Context, id: u32) -> u32 {
    let context = ManuallyDrop::new(Box::from_raw(ptr));
    let id = Id::from(id as usize);
    context.runner.egraph[id].nodes.len() as u32
}

#[no_mangle]
pub unsafe extern "C" fn egraph_enode_size(ptr: *mut Context, id: u32, idx: u32) -> u32 {
    let context = ManuallyDrop::new(Box::from_raw(ptr));
    let id = Id::from(id as usize);
    let idx = idx as usize;
    context.runner.egraph[id].nodes[idx].len() as u32
}

#[no_mangle]
pub unsafe extern "C" fn egraph_get_eclasses(ptr: *mut Context, ids_ptr: *mut u32) {
    let context = ManuallyDrop::new(Box::from_raw(ptr));
    let mut ids: Vec<u32> = context
        .runner
        .egraph
        .classes()
        .map(|c| usize::from(c.id) as u32)
        .collect();
    ids.sort();

    for (i, id) in ids.iter().enumerate() {
        std::ptr::write(ids_ptr.offset(i as isize), *id);
    }
}

#[no_mangle]
pub unsafe extern "C" fn egraph_get_node(
    ptr: *mut Context,
    id: u32,
    idx: u32,
    ids: *mut u32,
) -> *const c_char {
    let context = ManuallyDrop::new(Box::from_raw(ptr));
    let id = Id::from(id as usize);
    let idx = idx as usize;

    let node = &context.runner.egraph[id].nodes[idx];
    for (i, id) in node.children().iter().enumerate() {
        std::ptr::write(ids.offset(i as isize), usize::from(*id) as u32);
    }

    let c_string = ManuallyDrop::new(CString::new(node.to_string()).unwrap());
    c_string.as_ptr()
}

#[no_mangle]
pub unsafe extern "C" fn egraph_get_proof(
    ptr: *mut Context,
    expr: *const c_char,
    goal: *const c_char,
) -> *const c_char {
    // Safety: `ptr` was box allocated by `egraph_create`
    let mut context = ManuallyDrop::new(Box::from_raw(ptr));
    // Send `EGraph` since neither `Context` nor `Runner` are `Send`. `Runner::explain_equivalence` just forwards to `EGraph::explain_equivalence` so this is fine.
    let egraph = &mut context.runner.egraph;
    let expr_rec = CStr::from_ptr(expr).to_str().unwrap().parse().unwrap();
    let goal_rec = CStr::from_ptr(goal).to_str().unwrap().parse().unwrap();

    // extract the proof as a tree
    let string = egraph
        .explain_equivalence(&expr_rec, &goal_rec)
        .get_string_with_let()
        .replace('\n', " ");

    let c_string = ManuallyDrop::new(CString::new(string).unwrap());
    c_string.as_ptr()
}

#[no_mangle]
pub unsafe extern "C" fn egraph_is_unsound_detected(ptr: *mut Context) -> bool {
    // Safety: `ptr` was box allocated by `egraph_create`
    let context = ManuallyDrop::new(Box::from_raw(ptr));

    context
        .runner
        .egraph
        .analysis
        .unsound
        .load(Ordering::SeqCst)
}

#[no_mangle]
pub unsafe extern "C" fn egraph_get_cost(ptr: *mut Context, node_id: u32, iter: u32) -> u32 {
    // Safety: `ptr` was box allocated by `egraph_create`
    let context = ManuallyDrop::new(Box::from_raw(ptr));
    let ext = find_extracted(&context.runner, node_id, iter);

    ext.cost as u32
}

fn build_best(
    egraph: &EGraph,
    best: &HashMap<u32, (usize, Math)>,
    expr: &mut RecExpr,
    seen: &mut HashMap<Id, Id>,
    id: Id,
) -> Id {
    let id = egraph.find(id);
    if let Some(&expr_id) = seen.get(&id) {
        return expr_id;
    }

    let mut node = best[&(usize::from(id) as u32)].1.clone();
    node.update_children(|child| build_best(egraph, best, expr, seen, child));
    let expr_id = expr.add(node);
    seen.insert(id, expr_id);
    expr_id
}

fn batch_node_string(node: &Math) -> String {
    if node.is_leaf() {
        node.to_string()
    } else {
        let op = match node {
            Math::Other(op, _) => op.to_string(),
            _ => node.to_string(),
        };
        let children = node
            .children()
            .iter()
            .map(|id| usize::from(*id).to_string())
            .collect::<Vec<_>>()
            .join(" ");
        format!("({} {})", op, children)
    }
}

#[no_mangle]
pub unsafe extern "C" fn egraph_extract_best_batch(
    ptr: *mut Context,
    ids_ptr: *const u32,
    num_ids: u32,
) -> *const c_char {
    let timing = timing_enabled();
    let started = timing.then(Instant::now);
    let mut context = ManuallyDrop::new(Box::from_raw(ptr));
    let egraph_nodes = timing.then(|| context.runner.egraph.total_size());
    let egraph_classes = timing.then(|| context.runner.egraph.number_of_classes());
    let mut costs_ms = 0.0;
    let mut best_nodes_ms = 0.0;
    if context.best.is_none() {
        let costs_started = timing.then(Instant::now);
        let best = {
            let extractor =
                Extractor::new(&context.runner.egraph, AltCost::new(&context.runner.egraph));
            costs_ms = costs_started
                .map_or(0.0, |started| started.elapsed().as_secs_f64() * 1000.0);
            let best_nodes_started = timing.then(Instant::now);
            let best = context
                .runner
                .egraph
                .classes()
                .map(|eclass| {
                    let cost = extractor.find_best_cost(eclass.id);
                    let best = extractor.find_best_node(eclass.id).clone();
                    (usize::from(eclass.id) as u32, (cost, best))
                })
                .collect::<HashMap<_, _>>();
            best_nodes_ms = best_nodes_started
                .map_or(0.0, |started| started.elapsed().as_secs_f64() * 1000.0);
            best
        };
        context.best = Some(best);
    }
    let roots_started = timing.then(Instant::now);
    let ids = slice::from_raw_parts(ids_ptr, num_ids as usize);
    let mut expr = RecExpr::default();
    let mut seen = HashMap::new();
    let roots = ids
        .iter()
        .map(|id| {
            let id = context.runner.egraph.find(Id::from(*id as usize));
            let (cost, _) = context.best.as_ref().unwrap()[&(usize::from(id) as u32)].clone();
            if cost == usize::MAX {
                None
            } else {
                Some((cost, build_best(
                    &context.runner.egraph,
                    context.best.as_ref().unwrap(),
                    &mut expr,
                    &mut seen,
                    id,
                )))
            }
        })
        .collect::<Vec<_>>();
    let roots_ms = roots_started
        .map_or(0.0, |started| started.elapsed().as_secs_f64() * 1000.0);
    let missing_roots = roots.iter().filter(|root| root.is_none()).count();
    let expr_nodes = expr.as_ref().len();
    let serialize_started = timing.then(Instant::now);
    let roots = roots
        .iter()
        .map(|root| match root {
            Some((cost, id)) => format!("({} {})", cost, usize::from(*id)),
            None => "#f".to_string(),
        })
        .collect::<Vec<_>>()
        .join(" ");
    let nodes = expr
        .as_ref()
        .iter()
        .map(batch_node_string)
        .collect::<Vec<_>>()
        .join(" ");
    let output = format!("(({}) ({}))", roots, nodes);
    let output_bytes = output.len();
    let output = CString::new(output).unwrap();
    let serialize_ms = serialize_started
        .map_or(0.0, |started| started.elapsed().as_secs_f64() * 1000.0);
    if let (Some(started), Some(egraph_nodes), Some(egraph_classes)) =
        (started, egraph_nodes, egraph_classes)
    {
        eprintln!(
            "EGG_TIMING batch_extract roots={} missing_roots={} nodes={} classes={} best_nodes={} expr_nodes={} costs_ms={:.3} best_nodes_ms={:.3} roots_ms={:.3} serialize_ms={:.3} output_bytes={} total_ms={:.3}",
            num_ids,
            missing_roots,
            egraph_nodes,
            egraph_classes,
            context.best.as_ref().unwrap().len(),
            expr_nodes,
            costs_ms,
            best_nodes_ms,
            roots_ms,
            serialize_ms,
            output_bytes,
            started.elapsed().as_secs_f64() * 1000.0,
        );
    }
    CString::into_raw(output)
}
