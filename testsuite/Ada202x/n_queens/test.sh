
# The locale specified by the environment affects sort order.
export Set LC_ALL=C

RUN="a2xi ../aaa.a2i"
$RUN n_queens.a2x -command Test_N_Queens 8
$RUN n_queens.a2x -command Test_N_Queens 6
$RUN n_queens.a2x -command Test_N_Queens 4
