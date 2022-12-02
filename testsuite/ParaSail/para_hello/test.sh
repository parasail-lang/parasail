# The locale specified by the environment affects sort order.
export Set LC_ALL=C
RUN="psli ../aaa.psi"

# This test is currently disabled because, although it works fine, it does
# not produce an stable output for the testsuite
#   $RUN para_hello.psl -command Para_Hello

$RUN para_hello.psl -command Para_Hello2
