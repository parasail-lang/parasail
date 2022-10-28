
# The locale specified by the environment affects sort order.
export Set LC_ALL=C

RUN="skli ../aaa.ski"
$RUN parse_tree2.skl -command Test_Parse2 | sort
