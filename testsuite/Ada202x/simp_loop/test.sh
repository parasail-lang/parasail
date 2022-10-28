
# The locale specified by the environment affects sort order.
export Set LC_ALL=C

a2xi ../aaa.a2i simp_loop.a2x -command Test_Simp_Loop 10 | sort
