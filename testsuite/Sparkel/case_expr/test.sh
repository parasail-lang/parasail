
# The locale specified by the environment affects sort order.
export Set LC_ALL=C

RUN="skli ../aaa.ski"
$RUN case_expr.skl -command Case_Expr 1 2 3
