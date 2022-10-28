
# The locale specified by the environment affects sort order.
export Set LC_ALL=C

RUN="a2xi ../aaa.a2i"
$RUN for_loop.a2x -command Test_For_Loop 1 5 | sort
