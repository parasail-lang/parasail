
# The locale specified by the environment affects sort order.
export Set LC_ALL=C

RUN="skli ../aaa.ski"
$RUN simple_fib.skl -command Test 10

