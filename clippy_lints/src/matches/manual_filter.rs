use clippy_utils::diagnostics::span_lint_and_sugg;
use clippy_utils::source::snippet_with_applicability;
use clippy_utils::ty::is_type_diagnostic_item;
use clippy_utils::{is_lang_ctor, match_def_path};
use rustc_errors::Applicability;
use rustc_hir::LangItem::OptionNone;
use rustc_hir::{Arm, BindingAnnotation, Expr, ExprKind, Pat, PatKind};
use rustc_lint::LateContext;
use rustc_span::{sym, SyntaxContext};
use clippy_utils::sugg::Sugg;

use super::manual_map::{try_parse_pattern, OptionPat};
use super::needless_match::pat_same_as_expr;
use super::MANUAL_FILTER;

type ArmInfo<'tcx> = (OptionPat<'tcx>, Option<FilterCond<'tcx>>);

fn handle_arm<'tcx>(cx: &LateContext<'tcx>, arm: &'tcx Arm<'_>, ctxt: SyntaxContext) -> Option<ArmInfo<'tcx>> {
    let option_pat = try_parse_pattern(cx, arm.pat, ctxt);
    if_chain! {
        if let Some(OptionPat::Some { .. }) = option_pat;
        if arm.guard.is_none();
        if let ExprKind::Block(block, None) = arm.body.kind;
        if block.stmts.is_empty();
        if let Some(block_expr) = block.expr;
        if let ExprKind::If(cond, then_expr, Some(else_expr)) = block_expr.kind;
        if let Some((then_option_cond, else_option_cond))
            = handle_if_or_else_expr(cx, arm.pat, then_expr).zip(handle_if_or_else_expr(cx, arm.pat, else_expr));
        if then_option_cond != else_option_cond;
        then {
            let inverted = then_option_cond == OptionCond::None;
            let filter_cond = FilterCond {
                inverted,
                cond
            };
            return option_pat.map(|x| (x, Some(filter_cond)))
        }
    }
    option_pat.map(|x| (x, None))
}

// see doc for `handle_if_or_else_expr`
#[derive(PartialEq, Eq, Debug, Clone, Hash, Copy)]
enum OptionCond {
    None,
    Some,
}

// function called for each <ifelse> expression:
// Some(x) => if <cond> {
//    <ifelse>
// } else {
//    <ifelse>
// }
// If <ifelse> resolves to `Some(x)` return Some(OptionCond::Some)
// If <ifelse> resolves to `None, return Some(OptionCond::None)
// If the expression is something else return `None`
fn handle_if_or_else_expr<'tcx>(
    cx: &LateContext<'_>,
    pat: &Pat<'_>,
    if_or_else_expr: &'tcx Expr<'_>,
) -> Option<OptionCond> {
    if_chain! {
        if let ExprKind::Block(block, None) = if_or_else_expr.kind;
        if block.stmts.is_empty();
        if let Some(inner_if_or_else_expr) = block.expr;
        then {
        if pat_same_as_expr(pat, inner_if_or_else_expr) {
            return Some(OptionCond::Some)
        } else if let ExprKind::Path(ref then_qpath) = inner_if_or_else_expr.kind {
                if is_lang_ctor(cx, then_qpath, OptionNone) {
                    return Some(OptionCond::None);
                }
            }
        }
    }
    None
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

pub(crate) fn check<'tcx>(cx: &LateContext<'tcx>, ex: &Expr<'_>, arms: &'tcx [Arm<'_>], expr: &Expr<'_>) {
    let expr_ctxt = expr.span.ctxt(); // what for?
    if_chain! {
        let ty = cx.typeck_results().expr_ty(expr);
        if is_type_diagnostic_item(cx, ty, sym::Option);
        if arms.len() == 2;
        if let Some((arm_some, _))
            = handle_arm(cx, &arms[0], expr_ctxt).zip(handle_arm(cx, &arms[1], expr_ctxt))
              .map(arm_some_first_arm_none_second);
        if let (OptionPat::Some {pattern, ..}, Some(filter_cond)) = arm_some;
        if let PatKind::Binding(BindingAnnotation::Unannotated, _, name, None) = pattern.kind;
        then {
            let mut app = Applicability::MaybeIncorrect;
            let var_str = snippet_with_applicability(cx, ex.span, "..", &mut app);
            let cond_str = filter_cond.to_string(cx, &mut app);
            span_lint_and_sugg(cx,
                MANUAL_FILTER,
                expr.span,
                "manual implementation of `Option::filter`",
                "try",
                format!("{}.filter(|&{}| {})", var_str, name.name, cond_str),
                app
            )
        }
    }
}
