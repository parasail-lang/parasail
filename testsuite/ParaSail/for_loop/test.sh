# The locale specified by the environment affects sort order.
export Set LC_ALL=C
RUN="psli ../aaa.psi"

$RUN for_loop.psl -command Test_For_Loop 1 5
