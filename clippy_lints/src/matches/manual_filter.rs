use clippy_utils::diagnostics::span_lint_and_sugg;
use clippy_utils::match_qpath;
use clippy_utils::source::snippet_with_applicability;
use clippy_utils::ty::{is_type_diagnostic_item, same_type_and_consts};
use clippy_utils::{
    eq_expr_value, get_parent_expr_for_hir, get_parent_node, higher, is_else_clause, is_expr_path_def_path,
    is_lang_ctor, match_def_path, over, peel_blocks_with_stmt,
};
use rustc_errors::Applicability;
use rustc_hir::LangItem::{OptionNone, OptionSome};
use rustc_hir::{Arm, BinOpKind, BindingAnnotation, Expr, ExprKind, FnRetTy, Guard, Node, Pat, PatKind, Path};
use rustc_lint::LateContext;
use rustc_typeck::hir_ty_to_ty;

use rustc_span::sym;

use super::needless_match::pat_same_as_expr;
use super::MANUAL_FILTER;

pub(crate) fn check(cx: &LateContext<'_>, ex: &Expr<'_>, arms: &[Arm<'_>], expr: &Expr<'_>) {
    if_chain! {
        let ty = cx.typeck_results().expr_ty(expr);
        if is_type_diagnostic_item(cx, ty, sym::Option);
        if arms.len() == 2;
        if let PatKind::TupleStruct(ref qpath0, .. ) = &arms[0].pat.kind;
        if is_lang_ctor(cx, qpath0, OptionNone);
        // Check if the arm is equal to the body. Since we already know the arm `None`, the body is too
        if dbg!(pat_same_as_expr(arms[0].pat, arms[0].body));
        if arms[0].guard.is_none();
        // Now second arm
        // It needs to be of type
        // Some(x) => if cond {
        //    None
        // } else {
        //     x
        // }
        // `None` and `x` can be swapped
        if let PatKind::TupleStruct(ref qpath, fields, None) = &arms[1].pat.kind;
        if is_lang_ctor(cx, qpath, OptionSome);
        if fields.len() == 1; // TODO can probably be relaxed
        if let PatKind::Binding(BindingAnnotation::Unannotated, _, name, None) = fields[0].kind;
        if arms[1].guard.is_none();
        if let ExprKind::Block(block, None) = arms[1].body.kind;
        if block.stmts.is_empty();
        if let Some(block_expr) = block.expr;
        if let ExprKind::If(cond, then, Some(else_expr)) = block_expr.kind;
        if let ExprKind::Block(block1, None) = then.kind;
        if block1.stmts.is_empty();
        if let Some(then_expr) = block1.expr;
        if let ExprKind::Path(ref then_qpath) = then_expr.kind;
        if is_lang_ctor(cx, then_qpath, OptionNone);
        if let ExprKind::Block(block2, None) = else_expr.kind;
        if block2.stmts.is_empty();
        if let Some(trailing_expr2) = block2.expr;
        if let ExprKind::Call(func, args) = trailing_expr2.kind;
        if let ExprKind::Path(ref else_qpath) = func.kind;
        if is_lang_ctor(cx, else_qpath, OptionSome);
        if pat_same_as_expr(arms[1].pat, trailing_expr2);
        if args.len() == 1; // TODO can probably be relaxed
        if let ExprKind::Path(ref qpath7) = args[0].kind;
        if match_qpath(qpath7, &["x"]);
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
