use clippy_utils::{numeric_literal::NumericLiteral, source::snippet_opt};
use rustc_ast::LitKind;
use rustc_errors::Applicability;
use rustc_hir::{BinOpKind, Expr, ExprKind, Lit};
use rustc_lint::{LateContext, LateLintPass};
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
    pub CONFUSING_XOR_AND_POW,
    pedantic,
    "the `^` operator is being used as exponentiation"
}
declare_lint_pass!(ConfusingXorAndPow => [CONFUSING_XOR_AND_POW]);

impl LateLintPass<'_> for ConfusingXorAndPow {
    fn check_expr(&mut self, cx: &LateContext<'_>, expr: &Expr<'_>) {
        if_chain! {
            if let ExprKind::Binary(op, left, right) = &expr.kind;
            if op.node == BinOpKind::BitXor;
            if let ExprKind::Lit(litr) = &right.kind;
            if let ExprKind::Lit(litl) = &left.kind;
            if let snip_left = snippet_opt(cx, litl.span).unwrap();
            if let snip_right = snippet_opt(cx, litr.span).unwrap();
            if get_numlit(litr, &snip_right)
                        .unwrap()
                        .is_decimal();
            if get_numlit(litl, &snip_left)
                        .unwrap()
                        .is_decimal();
                then {

                            let left_val = unwrap_lit_to_dec(&left.kind).unwrap_or(0);
                            let right_val = unwrap_lit_to_dec(&right.kind).unwrap_or(0);
                            let suffix: &str = get_numlit( litr, &snip_right).unwrap().suffix.unwrap_or("");
                            if left_val == 2 &&
                             (right_val == 8 ||
                            right_val == 16 ||
                            right_val == 32 ||
                            right_val == 64)
                            {
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
                                    format!("{left_val}{suffix}.pow({right_val})"),
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

fn get_numlit<'a>(lit: &Lit, snip: &'a str) -> Option<NumericLiteral<'a>> {
    NumericLiteral::from_lit_kind(snip, &lit.node)
}

// fn get_suffix<'a>(cx: &LateContext<'_>, span: Span, lit: &Lit) -> Option<&'a str> {
//     let snippet = snippet_opt(cx, span);
//     let decoded = NumericLiteral::from_lit_kind(&snippet.unwrap_or(String::new()),
// &lit.node).unwrap(); 	decoded.suffix
// }
