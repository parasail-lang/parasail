# The locale specified by the environment affects sort order.
export Set LC_ALL=C

RUN="pryi ../aaa.pri"
$RUN table.pry -command Table 10 | sort
