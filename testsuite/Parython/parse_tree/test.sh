# The locale specified by the environment affects sort order.
export Set LC_ALL=C

RUN="pryi ../aaa.pri"
$RUN parse_tree.pry -command Test_Parse | sort
