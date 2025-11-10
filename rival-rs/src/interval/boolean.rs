use super::value::{Endpoint, ErrorFlags, Ival};
use rug::Assign;
use std::cmp::Ordering;

impl Ival {
    pub fn not_assign(&mut self, input: &Ival) {
        set_bool_result(
            self,
            BoolFlag::new(!endpoint_truth(&input.hi), input.hi.immovable),
            BoolFlag::new(!endpoint_truth(&input.lo), input.lo.immovable),
        );
        self.err = input.err;
    }

    pub fn and_assign(&mut self, lhs: &Ival, rhs: &Ival) {
        set_bool_result(
            self,
            BoolFlag::new(
                endpoint_truth(&lhs.lo) && endpoint_truth(&rhs.lo),
                lhs.lo.immovable && rhs.lo.immovable,
            ),
            BoolFlag::new(
                endpoint_truth(&lhs.hi) && endpoint_truth(&rhs.hi),
                lhs.hi.immovable && rhs.hi.immovable,
            ),
        );
        self.err = lhs.err.union(&rhs.err);
    }

    pub fn or_assign(&mut self, lhs: &Ival, rhs: &Ival) {
        set_bool_result(
            self,
            BoolFlag::new(
                endpoint_truth(&lhs.lo) || endpoint_truth(&rhs.lo),
                lhs.lo.immovable && rhs.lo.immovable,
            ),
            BoolFlag::new(
                endpoint_truth(&lhs.hi) || endpoint_truth(&rhs.hi),
                lhs.hi.immovable && rhs.hi.immovable,
            ),
        );
        self.err = lhs.err.union(&rhs.err);
    }

    pub fn eq_assign(&mut self, lhs: &Ival, rhs: &Ival) {
        let (can_lt, must_lt, can_gt, must_gt) = cmp_flags(lhs, rhs);
        set_bool_result(
            self,
            BoolFlag::new(
                !can_lt.value && !can_gt.value,
                can_lt.immovable && can_gt.immovable,
            ),
            BoolFlag::new(
                !must_lt.value && !must_gt.value,
                must_lt.immovable && must_gt.immovable,
            ),
        );
        self.err = lhs.err.union(&rhs.err);
    }

    pub fn ne_assign(&mut self, lhs: &Ival, rhs: &Ival) {
        let (can_lt, must_lt, can_gt, must_gt) = cmp_flags(lhs, rhs);
        set_bool_result(
            self,
            BoolFlag::new(
                must_lt.value || must_gt.value,
                must_lt.immovable && must_gt.immovable,
            ),
            BoolFlag::new(
                can_lt.value || can_gt.value,
                can_lt.immovable && can_gt.immovable,
            ),
        );
        self.err = lhs.err.union(&rhs.err);
    }

    pub fn lt_assign(&mut self, lhs: &Ival, rhs: &Ival) {
        let (can_lt, must_lt, _, _) = cmp_flags(lhs, rhs);
        set_bool_result(self, must_lt, can_lt);
        self.err = lhs.err.union(&rhs.err);
    }

    pub fn le_assign(&mut self, lhs: &Ival, rhs: &Ival) {
        let (_, _, can_gt, must_gt) = cmp_flags(lhs, rhs);
        set_bool_result(
            self,
            BoolFlag::new(!can_gt.value, can_gt.immovable),
            BoolFlag::new(!must_gt.value, must_gt.immovable),
        );
        self.err = lhs.err.union(&rhs.err);
    }

    pub fn gt_assign(&mut self, lhs: &Ival, rhs: &Ival) {
        let (_, _, can_gt, must_gt) = cmp_flags(lhs, rhs);
        set_bool_result(self, must_gt, can_gt);
        self.err = lhs.err.union(&rhs.err);
    }

    pub fn ge_assign(&mut self, lhs: &Ival, rhs: &Ival) {
        let (can_lt, must_lt, _, _) = cmp_flags(lhs, rhs);
        set_bool_result(
            self,
            BoolFlag::new(!can_lt.value, can_lt.immovable),
            BoolFlag::new(!must_lt.value, must_lt.immovable),
        );
        self.err = lhs.err.union(&rhs.err);
    }

    pub fn if_assign(&mut self, cond: &Ival, when_true: &Ival, when_false: &Ival) {
        let cond_lo_true = endpoint_truth(&cond.lo);
        let cond_hi_true = endpoint_truth(&cond.hi);
        let cond_err = cond.err;

        let mut assign_branch = |branch: &Ival| {
            self.assign_from(branch);
            self.err = cond_err.union(&branch.err);
        };

        match (cond_lo_true, cond_hi_true) {
            (true, _) => assign_branch(when_true),
            (_, false) => assign_branch(when_false),
            _ => {
                assign_union(self, when_true, when_false);
                self.err = cond_err.union(&self.err);
                if !(cond.lo.immovable && cond.hi.immovable) {
                    self.lo.immovable = false;
                    self.hi.immovable = false;
                }
            }
        }
    }

    pub fn error_assign(&mut self, input: &Ival) {
        set_bool_result(
            self,
            BoolFlag::new(input.err.total, false),
            BoolFlag::new(input.err.partial, false),
        );
        self.err = ErrorFlags::none();
    }

    pub fn assert_assign(&mut self, cond: &Ival) {
        set_bool_result(self, BoolFlag::always_true(), BoolFlag::always_true());
        self.err = ErrorFlags::new(
            cond.err.partial || !endpoint_truth(&cond.lo),
            cond.err.total || !endpoint_truth(&cond.hi),
        );
    }
}

#[derive(Clone, Copy)]
struct BoolFlag {
    value: bool,
    immovable: bool,
}

impl BoolFlag {
    fn new(value: bool, immovable: bool) -> Self {
        BoolFlag { value, immovable }
    }
    fn always_true() -> Self {
        BoolFlag::new(true, true)
    }
    fn assign(self, endpoint: &mut Endpoint) {
        endpoint
            .as_float_mut()
            .assign(if self.value { 1 } else { 0 });
        endpoint.immovable = self.immovable;
    }
}

fn set_bool_result(out: &mut Ival, lo: BoolFlag, hi: BoolFlag) {
    lo.assign(&mut out.lo);
    hi.assign(&mut out.hi);
}

fn endpoint_truth(ep: &Endpoint) -> bool {
    !ep.as_float().is_zero()
}

fn assign_union(out: &mut Ival, a: &Ival, b: &Ival) {
    if a.err.total {
        out.assign_from(b);
        out.err.partial = true;
        return;
    }

    if b.err.total {
        out.assign_from(a);
        out.err.partial = true;
        return;
    }

    // Helper to assign endpoint based on comparison
    let assign_endpoint = |out_ep: &mut super::value::Endpoint,
                           a_ep: &super::value::Endpoint,
                           b_ep: &super::value::Endpoint,
                           prefer_smaller: bool| {
        match a_ep.val.cmp(&b_ep.val) {
            Ordering::Equal => {
                out_ep.as_float_mut().assign(a_ep.as_float());
                out_ep.immovable = a_ep.immovable || b_ep.immovable;
            }
            Ordering::Less if prefer_smaller => {
                out_ep.as_float_mut().assign(a_ep.as_float());
                out_ep.immovable = a_ep.immovable;
            }
            Ordering::Greater if !prefer_smaller => {
                out_ep.as_float_mut().assign(a_ep.as_float());
                out_ep.immovable = a_ep.immovable;
            }
            _ => {
                out_ep.as_float_mut().assign(b_ep.as_float());
                out_ep.immovable = b_ep.immovable;
            }
        }
    };

    assign_endpoint(&mut out.lo, &a.lo, &b.lo, true);
    assign_endpoint(&mut out.hi, &a.hi, &b.hi, false);

    out.err = a.err.union_disjoint(&b.err);
}

fn cmp_flags(lhs: &Ival, rhs: &Ival) -> (BoolFlag, BoolFlag, BoolFlag, BoolFlag) {
    let can_lt = BoolFlag::new(
        lhs.lo.as_float() < rhs.hi.as_float(),
        lhs.lo.immovable && rhs.hi.immovable,
    );
    let must_lt = BoolFlag::new(
        lhs.hi.as_float() < rhs.lo.as_float(),
        lhs.hi.immovable && rhs.lo.immovable,
    );
    let can_gt = BoolFlag::new(
        lhs.hi.as_float() > rhs.lo.as_float(),
        lhs.hi.immovable && rhs.lo.immovable,
    );
    let must_gt = BoolFlag::new(
        lhs.lo.as_float() > rhs.hi.as_float(),
        lhs.lo.immovable && rhs.hi.immovable,
    );

    (can_lt, must_lt, can_gt, must_gt)
}
