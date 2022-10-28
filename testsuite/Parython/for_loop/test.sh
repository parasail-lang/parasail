# The locale specified by the environment affects sort order.
export Set LC_ALL=C

RUN="pryi ../aaa.pri"
$RUN for_loop.pry -command Test_For_Loop 1 5 | sort
