
# The locale specified by the environment affects sort order.
export Set LC_ALL=C

RUN="a2xi ../aaa.a2i"
$RUN simple_hello_world.a2x -command Hello_World
