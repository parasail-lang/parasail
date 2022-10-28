
# The locale specified by the environment affects sort order.
export Set LC_ALL=C

RUN="skli ../aaa.ski"
$RUN table.skl -command Table 10 | sort
