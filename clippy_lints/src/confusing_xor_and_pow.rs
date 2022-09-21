use clippy_utils::{numeric_literal::NumericLiteral, source::snippet_opt};
use rustc_ast::LitKind;
use rustc_errors::Applicability;
use rustc_hir::{BinOpKind, Expr, ExprKind, Lit};
use rustc_lint::{LateContext, LateLintPass};
use rustc_session::{declare_lint_pass, declare_tool_lint};
use rustc_span::Span;

declare_clippy_lint! {
    /// ### What it does
    /// Warns for a Bitwise XOR operator being probably confused as a powering. It will not trigger if any of the numbers are not in decimal.
    /// ### Why is this bad?
    ///	It's most probably a typo and may lead to unexpected behaviours.
    /// ### Example
    /// ```rust
    /// let x = 3 ^ 4;
    /// ```
    /// Use instead:
    /// ```rust
    /// let x = 3.pow(4);
    /// ```
    #[clippy::version = "1.65.0"]
    pub CONFUSING_XOR_AND_POW,
    pedantic,
    "default lint description"
}
declare_lint_pass!(ConfusingXorAndPow => [CONFUSING_XOR_AND_POW]);

impl LateLintPass<'_> for ConfusingXorAndPow {
    fn check_expr(&mut self, cx: &LateContext<'_>, expr: &Expr<'_>) {
        if_chain! {
            if let ExprKind::Binary(op, left, right) = &expr.kind;
            if op.node == BinOpKind::BitXor;
            if let ExprKind::Lit(litr) = &right.kind;
            if let ExprKind::Lit(litl) = &left.kind;
            if is_decimal(cx, right.span, litr) && is_decimal(cx, left.span, litl);
            then {
                let left_val = unwrap_lit_to_dec(&left.kind).unwrap_or(0);
                let right_val = unwrap_lit_to_dec(&right.kind).unwrap_or(0);
                if left_val == 2 && (right_val == 8 || right_val == 16 || right_val == 32 || right_val == 64) {
                    clippy_utils::diagnostics::span_lint_and_sugg(
                        cx,
                        CONFUSING_XOR_AND_POW,
                        expr.span,
                        "it appears that you are trying to get the maximum value of an integer, but '^' is not exponentiation operator",
                        "try with",
                        format!("u{right_val}::MAX"),
                        Applicability::MaybeIncorrect,
                    );
                } else {
                    // Even then, warn always.
                    clippy_utils::diagnostics::span_lint_and_sugg(
                        cx,
                        CONFUSING_XOR_AND_POW,
                        expr.span,
                        "'^' is not the exponentiation operator",
                        "did you mean to write",
                        format!("{left_val}.pow({right_val})"),
                        Applicability::MaybeIncorrect,
                    );
                }
            }
        }
    }
}

// from ExprKind::Lit.node (Int) to u128
fn unwrap_lit_to_dec(expr: &ExprKind<'_>) -> Option<u128> {
    match expr {
        ExprKind::Lit(lit) => match lit.node {
            LitKind::Int(num, _) => Some(num),
            _ => None,
        },
        _ => None,
    }
}

fn is_decimal(cx: &LateContext<'_>, span: Span, lit: &Lit) -> bool {
    if let Some(snippet) = snippet_opt(cx, span) {
        if let Some(decoded) = NumericLiteral::from_lit_kind(&snippet, &lit.node) {
            return decoded.is_decimal();
        }
    }
    false
}
