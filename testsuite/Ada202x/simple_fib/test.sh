
# The locale specified by the environment affects sort order.
export Set LC_ALL=C

RUN="a2xi ../aaa.a2i"
$RUN simple_fib.a2x -command Test 10

