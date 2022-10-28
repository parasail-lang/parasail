# The locale specified by the environment affects sort order.
export Set LC_ALL=C

RUN="pryi ../aaa.pri"
$RUN simp_loop.pry -command Test_Simp_Loop 10 | sort
