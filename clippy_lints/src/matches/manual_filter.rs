use clippy_utils::diagnostics::span_lint_and_sugg;
use clippy_utils::is_lang_ctor;
use clippy_utils::source::snippet_with_applicability;
use clippy_utils::ty::is_type_diagnostic_item;
use rustc_errors::Applicability;
use rustc_hir::LangItem::{OptionNone, OptionSome};
use rustc_hir::{Arm, BindingAnnotation, Expr, ExprKind, Pat, PatKind};
use rustc_lint::LateContext;

use rustc_span::{sym, SyntaxContext};

use super::manual_map::{try_parse_pattern, OptionPat};
use super::needless_match::pat_same_as_expr;
use super::MANUAL_FILTER;

// TODO doc
fn handle_arm<'ctx>(cx: &LateContext<'ctx>, arm: &'ctx Arm<'_>, ctxt: SyntaxContext) -> Option<OptionPat<'ctx>> {
    let option_pat = try_parse_pattern(cx, arm.pat, ctxt);
    if let Some(OptionPat::Some { .. }) = option_pat {
        todo!()
    };
    todo!()
}

// Contains the cond part of the snippet below
// if `inverted` is set to `true`, then `x` and `None` are swapped 
// if cond {
//    x
// } else {
//    None
// }
struct FilterCond<'tcx> {
    cond: &'tcx Expr<'tcx>,
    inverted: bool,
}

impl FilterCond<'_> {
    fn to_string(self, cx: &LateContext<'_>, app: &mut Applicability) -> String {
        format!(
            "{}{}",
            if self.inverted { "!" } else { "" },
            snippet_with_applicability(cx, self.cond.span, "..", app)
        )
    }
}

pub(crate) fn check(cx: &LateContext<'_>, ex: &Expr<'_>, arms: &[Arm<'_>], expr: &Expr<'_>) {
    let expr_ctxt = expr.span.ctxt(); // what for?
    if_chain! {
        let ty = cx.typeck_results().expr_ty(expr);
        if dbg!(is_type_diagnostic_item(cx, ty, sym::Option));
        if dbg!(arms.len() == 2);
        // Now second arm
        // It needs to be of type
        // Some(x) => if cond {
        //    None
        // } else {
        //     x
        // }
        // `None` and `x` can be swapped
        if let PatKind::TupleStruct(ref qpath, fields, None) = &arms[1].pat.kind;
        if dbg!(is_lang_ctor(cx, qpath, OptionSome));
        if fields.len() == 1; // TODO can probably be relaxed
        if let PatKind::Binding(BindingAnnotation::Unannotated, _, name, None) = fields[0].kind;
        if dbg!(arms[1].guard.is_none());
        if let ExprKind::Block(block, None) = arms[1].body.kind;
        if dbg!(block.stmts.is_empty());
        if let Some(block_expr) = block.expr;
        if let ExprKind::If(cond, then, Some(else_expr)) = block_expr.kind;
        if let ExprKind::Block(block1, None) = then.kind;
        if dbg!(block1.stmts.is_empty());
        if let Some(then_expr) = block1.expr;
        if let ExprKind::Path(ref then_qpath) = then_expr.kind;
        if dbg!(is_lang_ctor(cx, then_qpath, OptionNone));
        if let ExprKind::Block(block2, None) = else_expr.kind;
        if dbg!(block2.stmts.is_empty());
        if let Some(trailing_expr2) = block2.expr;
        if let ExprKind::Call(func, args) = trailing_expr2.kind;
        if let ExprKind::Path(ref else_qpath) = func.kind;
        if dbg!(is_lang_ctor(cx, else_qpath, OptionSome));
        if dbg!(pat_same_as_expr(arms[1].pat, trailing_expr2));
        if dbg!(args.len() == 1); // TODO can probably be relaxed

        if let PatKind::Path(ref qpath0) = &arms[0].pat.kind;
        if dbg!(is_lang_ctor(cx, qpath0, OptionNone));
        // Check if the arm is equal to the body. Since we already know the arm `None`, the body is too
        if dbg!(pat_same_as_expr(arms[0].pat, arms[0].body));
        if arms[0].guard.is_none();
        then {
            let mut app = Applicability::MaybeIncorrect;
            let var_str = snippet_with_applicability(cx, ex.span, "..", &mut app);
            let cond_str = snippet_with_applicability(cx, cond.span, "..", &mut app);
            span_lint_and_sugg(cx,
                MANUAL_FILTER,
                expr.span,
                "manual implementation of `Option::filter`",
                "try",
                format!("{}.filter(|{}| {})", var_str, name.name, cond_str),
                app
            )
        }
    }
}
