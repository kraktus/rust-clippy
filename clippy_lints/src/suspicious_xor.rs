use clippy_utils::{numeric_literal::NumericLiteral, source::snippet};
use rustc_ast::LitKind;
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
        if_chain! {
            if !in_external_macro(cx.sess(), expr.span);
            if let ExprKind::Binary(op, left, right) = &expr.kind;
            if op.node == BinOpKind::BitXor;
            if let ExprKind::Lit(litr) = &right.kind;
            if let ExprKind::Lit(litl) = &left.kind;
            if let snip_left = snippet(cx, litl.span, "..");
            if let snip_right = snippet(cx, litr.span, "..");
            if get_numlit(litr, &snip_right)
            .zip(get_numlit(litl, &snip_left))
            .map_or(false, |(a,b)| a.is_decimal() && b.is_decimal());
            if let left_val = unwrap_lit_to_dec(left).unwrap_or(0);
            if let right_val = unwrap_lit_to_dec(right).unwrap_or(0);
            if let suffix = get_numlit(litr, &snip_right).unwrap().suffix.unwrap_or("");
                then {

                            if left_val == 2 &&
                             (right_val == 8 ||
                            right_val == 16 ||
                            right_val == 32 ||
                            right_val == 64 || right_val == 128)
                            {
                                clippy_utils::diagnostics::span_lint_and_sugg(
                                    cx,
                                    SUSPICIOUS_XOR,
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
                                    SUSPICIOUS_XOR,
                                    expr.span,
                                    "'^' is not the exponentiation operator",
                                    "did you mean to write",
                                    format!("{left_val}{suffix}.pow({right_val})"),
                                    Applicability::MaybeIncorrect,
                                );
                            }

                    }
        }
    }
}

fn unwrap_lit_to_dec(expr: &Expr<'_>) -> Option<u128> {
    match &expr.kind {
        ExprKind::Lit(lit) => match lit.node {
            LitKind::Int(num, _) => Some(num),
            _ => None,
        },
        _ => None,
    }
}

fn get_numlit<'a>(lit: &Lit, snip: &'a str) -> Option<NumericLiteral<'a>> {
    NumericLiteral::from_lit_kind(snip, &lit.node)
}
