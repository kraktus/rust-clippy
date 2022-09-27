use clippy_utils::{numeric_literal::NumericLiteral, source::snippet};
use rustc_errors::Applicability;
use rustc_hir::{BinOpKind, Expr, ExprKind, Lit};
use rustc_lint::{LateContext, LateLintPass, LintContext};
use rustc_middle::lint::in_external_macro;
use rustc_session::{declare_lint_pass, declare_tool_lint};

declare_clippy_lint! {
    /// ### What it does
    /// Warns for a Bitwise XOR operator being probably confused as a powering. It will not trigger if any of the numbers are not in decimal.
    /// ### Why is this bad?
    ///	It's most probably a typo and may lead to unexpected behaviours.
    /// ### Example
    /// ```rust
    /// let x = 3_i32 ^ 4_i32;
    /// ```
    /// Use instead:
    /// ```rust
    /// let x = 3_i32.pow(4);
    /// ```
    #[clippy::version = "1.65.0"]
    pub SUSPICIOUS_XOR,
    suspicious,
    "XOR (`^`) operator possibly used as exponentiation operator"
}
declare_lint_pass!(ConfusingXorAndPow => [SUSPICIOUS_XOR]);

impl LateLintPass<'_> for ConfusingXorAndPow {
    fn check_expr(&mut self, cx: &LateContext<'_>, expr: &Expr<'_>) {
        if !in_external_macro(cx.sess(), expr.span) &&
            let ExprKind::Binary(op, left, right) = &expr.kind &&
            op.node == BinOpKind::BitXor &&
            let ExprKind::Lit(lit_left) = &left.kind &&
            let ExprKind::Lit(lit_right) = &right.kind &&
            let snip_left = snippet(cx, lit_left.span, "..") &&
            let snip_right = snippet(cx, lit_right.span, "..") &&
            let Some(left_val) = NumericLiteral::from_lit_kind(lit_left, &snip_left) &&
            let Some(right_val) = NumericLiteral::from_lit_kind(lit_right, &snip_right) &&
            left_val.is_decimal() && right_val.is_decimal() {
                clippy_utils::diagnostics::span_lint_and_sugg(
                        cx,
                        SUSPICIOUS_XOR,
                        expr.span,
                        "'^' is not the exponentiation operator",
                        "did you mean to write",
                        format!("{}.pow({})", left_val.format(), right_val.format()),
                        Applicability::MaybeIncorrect,
                    );
            }
    }
}
