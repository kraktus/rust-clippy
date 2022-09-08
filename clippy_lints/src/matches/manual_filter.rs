use clippy_utils::diagnostics::span_lint_and_sugg;
use clippy_utils::source::snippet_with_applicability;
use clippy_utils::sugg::Sugg;
use clippy_utils::ty::is_type_diagnostic_item;
use clippy_utils::{is_lang_ctor, path_to_local_id};
use rustc_errors::Applicability;
use rustc_hir::intravisit::{walk_expr, Visitor};
use rustc_hir::LangItem::OptionNone;
use rustc_hir::LangItem::OptionSome;
use rustc_hir::{Arm, BindingAnnotation, Block, BlockCheckMode, Expr, ExprKind, HirId, Pat, PatKind, UnsafeSource};
use rustc_lint::LateContext;
use rustc_span::{sym, SyntaxContext};

use super::manual_map::{check_with, try_parse_pattern, OptionPat, SomeExpr};
use super::needless_match::pat_same_as_expr;
use super::MANUAL_FILTER;

type ArmInfo<'tcx> = (OptionPat<'tcx>, Option<FilterCond<'tcx>>);

// fn handle_arm<'tcx>(
//     cx: &LateContext<'tcx>,
//     pat_opt: Option<&'tcx Pat<'_>>,
//     body: &'tcx Expr<'_>,
//     ctxt: SyntaxContext,
// ) -> Option<ArmInfo<'tcx>> {
//     // the nested options are intentional here
//     let option_pat = pat_opt.map_or(Some(OptionPat::Wild), |pat| try_parse_pattern(cx, pat,
// ctxt));     if_chain! {
//         if let Some(OptionPat::Some { .. }) = option_pat;
//         if let Some(pat) = pat_opt; // Always some because it needs to some to  `option_pat:
// OptionPat::Some{..}`         if let ExprKind::Block(block, None) = body.kind;
//         if block.stmts.is_empty();
//         if let Some(block_expr) = block.expr;
//         if let ExprKind::If(cond, then_expr, Some(else_expr)) = block_expr.kind;
//         if let Some((then_option_cond, else_option_cond))
//             = handle_if_or_else_expr(cx, pat, then_expr).zip(handle_if_or_else_expr(cx, pat,
// else_expr));         if then_option_cond != else_option_cond;
//         then {
//             let inverted = then_option_cond == OptionCond::None;
//             let filter_cond = FilterCond {
//                 inverted,
//                 cond: cond.peel_drop_temps()
//             };
//             return option_pat.map(|x| (x, Some(filter_cond)))
//         }
//     }
//     option_pat.map(|x| (x, None))
// }

struct CondVisitor<'a, 'tcx> {
    cx: &'a LateContext<'tcx>,
    target: HirId,
    ctxt: SyntaxContext,
    is_equal_to_pat_bind: bool,
    needs_unsafe_block: bool,
}

impl<'a, 'tcx> CondVisitor<'a, 'tcx> {
    fn new(cx: &'a LateContext<'tcx>, target: HirId, ctxt: SyntaxContext) -> Self {
        Self {
            cx,
            target,
            ctxt,
            is_equal_to_pat_bind: false,
            needs_unsafe_block: false,
        }
    }
}

impl<'a, 'tcx> Visitor<'tcx> for CondVisitor<'a, 'tcx> {
    fn visit_expr(&mut self, expr: &'tcx Expr<'tcx>) {
        match expr.kind {
            ExprKind::Block(
                Block {
                    rules: BlockCheckMode::UnsafeBlock(UnsafeSource::UserProvided),
                    ..
                },
                _,
            ) => self.needs_unsafe_block = true,
            ExprKind::Call(
                Expr {
                    kind: ExprKind::Path(ref qpath),
                    ..
                },
                [arg],
            ) if self.ctxt == expr.span.ctxt() && is_lang_ctor(self.cx, qpath, OptionSome) => {
                if path_to_local_id(arg, self.target) {
                    self.is_equal_to_pat_bind = true;
                    return;
                }
            },
            _ => walk_expr(self, expr),
        }
    }
}

// Function called on the `expr` of `[&+]Some((ref | ref mut) x) => <expr>`
// Need to check if it's of the `if <cond> {<then_expr>} else {<else_expr>}`
// AND that only one `then/else_expr` resolves to `Some(x)` while the other resolves to `None`
// return `cond` if
fn get_cond_expr<'tcx>(
    cx: &LateContext<'tcx>,
    pat: &Pat<'_>,
    expr: &'tcx Expr<'_>,
    ctxt: SyntaxContext,
) -> Option<SomeExpr<'tcx>> {
    if_chain! {
        if let ExprKind::Block(block, None) = expr.kind;
        if block.stmts.is_empty();
        if let Some(block_expr) = block.expr;
        if let ExprKind::If(cond, then_expr, Some(else_expr)) = block_expr.kind;
        if let PatKind::Binding(_,target, ..) = pat.kind;
        if let (then_option_cond, else_option_cond)
            = (handle_if_or_else_expr(cx, target, ctxt, then_expr),
                handle_if_or_else_expr(cx, target, ctxt, else_expr));
        if then_option_cond != else_option_cond; // check that one expr resolves to `Some(x)`, the other to `None`
        then {
            // we only need the visitor to look for unsafe block.
            // TODO could probably be refactored not to need a visitor altogether
            let mut visitor_cond = CondVisitor::new(cx, target, ctxt);
            visitor_cond.visit_expr(block_expr);
            if dbg!(visitor_cond.is_equal_to_pat_bind) {
                Some(SomeExpr {
                    expr: cond.peel_drop_temps(),
                    needs_unsafe_block: visitor_cond.needs_unsafe_block,
                    needs_negated: !then_option_cond // if the `if_expr` resolves to `None`, we need to negate the cond
                })
            } else {
                None
            }
        } else {
            None
        }
    }
}

// see doc for `handle_if_or_else_expr`
// #[derive(PartialEq, Eq, Debug, Clone, Hash, Copy)]
// enum OptionCond {
//     None,
//     Some,
// }

// ASSUME we already know the expression resolves to an `Option`
// function called for each <ifelse> expression:
// Some(x) => if <cond> {
//    <ifelse>
// } else {
//    <ifelse>
// }
// If <ifelse> resolves to `Some(x)` return `true`
// otherwise `false
fn handle_if_or_else_expr<'tcx>(
    cx: &LateContext<'_>,
    target: HirId,
    ctxt: SyntaxContext,
    if_or_else_expr: &'tcx Expr<'_>,
) -> bool {
    // let mut visitor_cond = CondVisitor::new(cx, target, ctxt);
    if_chain! {
            if let ExprKind::Block(block, None) = if_or_else_expr.kind;
            if block.stmts.is_empty(); // could we handle side effects?
            if let Some(inner_if_or_else_expr) = block.expr;
            then {
            return path_to_local_id(inner_if_or_else_expr, target)
        }
    }
    false
}

// Contains the <cond> part of the snippet below
// if `inverted` is set to `true`, then `x` and `None` are swapped
// Some(x) => if <cond> {
//    x
// } else {
//    None
// }
#[derive(Debug)]
struct FilterCond<'tcx> {
    cond: &'tcx Expr<'tcx>,
    inverted: bool,
}

impl<'tcx> FilterCond<'tcx> {
    fn to_string(self, cx: &LateContext<'tcx>, app: &mut Applicability) -> String {
        let cond_sugg = Sugg::hir_with_applicability(cx, self.cond, "..", app);
        if self.inverted {
            format!("{}", !cond_sugg)
        } else {
            format!("{}", cond_sugg)
        }
    }
}

// return (arm1, arm2), no matter how the original match was defined
// <Some(x) => {...}> = arm1
// <None | _ => {...}> = arm2
fn arm_some_first_arm_none_second<'tcx>(
    (first_arm, second_arm): (ArmInfo<'tcx>, ArmInfo<'tcx>),
) -> (ArmInfo<'tcx>, ArmInfo<'tcx>) {
    if let (OptionPat::None | OptionPat::Wild, _) = second_arm {
        (first_arm, second_arm)
    } else {
        (second_arm, first_arm)
    }
}

pub(super) fn check_match<'tcx>(
    cx: &LateContext<'tcx>,
    scrutinee: &'tcx Expr<'_>,
    arms: &'tcx [Arm<'_>],
    expr: &'tcx Expr<'_>,
) {
    if_chain! {
        let ty = cx.typeck_results().expr_ty(expr);
        if is_type_diagnostic_item(cx, ty, sym::Option);
        if arms.len() == 2;
        if arms[0].guard.is_none();
        if arms[1].guard.is_none();
        then {
            check(cx, expr, scrutinee, &arms[0].pat, &arms[0].body, Some(&arms[1].pat), &arms[1].body)
        }
    }
}

pub(super) fn check_if_let<'tcx>(
    cx: &LateContext<'tcx>,
    expr: &'tcx Expr<'_>,
    let_pat: &'tcx Pat<'_>,
    let_expr: &'tcx Expr<'_>,
    then_expr: &'tcx Expr<'_>,
    else_expr: &'tcx Expr<'_>,
) {
    check(cx, expr, let_expr, let_pat, then_expr, None, else_expr);
}

fn check<'tcx>(
    cx: &LateContext<'tcx>,
    expr: &'tcx Expr<'_>,
    scrutinee: &'tcx Expr<'_>,
    then_pat: &'tcx Pat<'_>,
    then_body: &'tcx Expr<'_>,
    else_pat: Option<&'tcx Pat<'_>>,
    else_body: &'tcx Expr<'_>,
) {
    check_with(
        cx,
        expr,
        scrutinee,
        then_pat,
        then_body,
        else_pat,
        else_body,
        get_cond_expr,
        "filter",
    )
    // let expr_ctxt = expr.span.ctxt(); // what for?
    // if_chain! {
    //         if let Some((arm_some, _))
    //         = handle_arm(cx, Some(then_pat), then_body, expr_ctxt)
    //         .zip(handle_arm(cx, else_pat, else_body, expr_ctxt))
    //         .map(arm_some_first_arm_none_second);
    //     if let (OptionPat::Some {pattern, ..}, Some(filter_cond)) = arm_some;
    //     if let PatKind::Binding(BindingAnnotation::Unannotated, _, name, None) = pattern.kind;
    //     then {
    //         let mut app = Applicability::MaybeIncorrect;
    //         let var_str = snippet_with_applicability(cx, scrutinee.span, "..", &mut app);
    //         let cond_str = filter_cond.to_string(cx, &mut app);
    //         span_lint_and_sugg(cx,
    //             MANUAL_FILTER,
    //             expr.span,
    //             "manual implementation of `Option::filter`",
    //             "try",
    //             format!("{}.filter(|&{}| {})", var_str, name.name, cond_str),
    //             app
    //         )
    //     }
    // }
}
