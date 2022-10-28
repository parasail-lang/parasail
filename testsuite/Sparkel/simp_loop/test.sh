
# The locale specified by the environment affects sort order.
export Set LC_ALL=C

skli ../aaa.ski simp_loop.skl -command Test_Simp_Loop 10 | sort
