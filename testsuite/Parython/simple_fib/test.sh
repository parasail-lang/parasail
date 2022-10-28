# The locale specified by the environment affects sort order.
export Set LC_ALL=C

RUN="pryi ../aaa.pri"
$RUN simple_fib.pry -command Test_Fib 10
