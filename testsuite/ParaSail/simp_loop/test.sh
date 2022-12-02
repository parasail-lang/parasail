# The locale specified by the environment affects sort order.
export Set LC_ALL=C
RUN="psli ../aaa.psi"

$RUN simp_loop.psl -command Test_Simp_Loop 10 |
  sort
