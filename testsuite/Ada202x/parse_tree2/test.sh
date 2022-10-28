
# The locale specified by the environment affects sort order.
export Set LC_ALL=C

RUN="a2xi ../aaa.a2i"
$RUN parse_tree2.a2x -command Test_Parse2 | sort
