use clippy_utils::diagnostics::span_lint_and_sugg;
use clippy_utils::match_qpath;
use clippy_utils::source::snippet_with_applicability;
use clippy_utils::ty::{is_type_diagnostic_item, same_type_and_consts};
use clippy_utils::{
    eq_expr_value, get_parent_expr_for_hir, get_parent_node, higher, is_else_clause, is_expr_path_def_path,
    is_lang_ctor, match_def_path, over, peel_blocks_with_stmt,
};
use rustc_ast::ast::{LitIntType, LitKind};
use rustc_errors::Applicability;
use rustc_hir::LangItem::{OptionSome, OptionNone};
use rustc_hir::{Arm, BinOpKind, BindingAnnotation, Expr, ExprKind, FnRetTy, Guard, Node, Pat, PatKind, Path, QPath};
use rustc_lint::LateContext;
use rustc_typeck::hir_ty_to_ty;

use rustc_span::sym;

use super::MANUAL_FILTER;

// stripped version of `pat_same_as_expr` from `needless_match.rs`
// Example: `Custom::TypeA => Custom::TypeB`, or `None => None`
// In our case We're only interested in `None => None`
fn pat_and_expr_none(pat: &Pat<'_>, expr: &Expr<'_>) -> bool {
    match (&pat.kind, &expr.kind) {
        (PatKind::Path(QPath::Resolved(_, p_path)), ExprKind::Path(QPath::Resolved(_, e_path))) => {
            over(p_path.segments, e_path.segments, |p_seg, e_seg| {
                p_seg.ident.name == e_seg.ident.name
            })
        },
        _ => false,
    }
}

pub(crate) fn check(cx: &LateContext<'_>, ex: &Expr<'_>, arms: &[Arm<'_>], expr: &Expr<'_>) {
    if_chain! {
        let ty = cx.typeck_results().expr_ty(expr);
        if is_type_diagnostic_item(cx, ty, sym::Option);
        if arms.len() == 2;
        // if let PatKind::Path(ref qpath1) = arms[0].pat.kind;
        // Check if the arm (order is arbitrary) branch is kind `Custom::TypeA => Custom::TypeB`, or `None => None`
        // since we already know the total expression resolves to `Option` it must be `None => None`
        if pat_and_expr_none(arms[0].pat, arms[0].body);
        if arms[0].guard.is_none();
        // Now second arm
        // It needs to be of type
        // Some(x) => if cond {
        //    x
        // } else {
        //     None
        // }
        // `None` and `x` can be swapped
        if let PatKind::TupleStruct(ref qpath, fields, None) = &arms[1].pat.kind;
        if is_lang_ctor(cx, qpath, OptionSome);
        if fields.len() == 1; // TODO can probably be relaxed
        if let PatKind::Binding(BindingAnnotation::Unannotated, _, name, None) = fields[0].kind; // name = x
        if arms[1].guard.is_none();
        //
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
        if args.len() == 1;
        if let ExprKind::Path(ref qpath7) = args[0].kind;
        if match_qpath(qpath7, &["x"]);
        then {
                    return;
        }
    }
}
