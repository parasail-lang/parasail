# The locale specified by the environment affects sort order.
export Set LC_ALL=C
RUN="psli ../aaa.psi"

($RUN simp_fib3.psl -command main 2>&1) > output
cat output | sort
rm output
