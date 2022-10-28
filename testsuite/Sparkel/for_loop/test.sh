
# The locale specified by the environment affects sort order.
export Set LC_ALL=C

RUN="skli ../aaa.ski"
$RUN for_loop.skl -command Test_For_Loop 1 5 | sort
