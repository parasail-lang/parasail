
# The locale specified by the environment affects sort order.
export Set LC_ALL=C

RUN="skli ../aaa.ski"
$RUN n_queens.skl -command Test_N_Queens 8
$RUN n_queens.skl -command Test_N_Queens 6
$RUN n_queens.skl -command Test_N_Queens 4
