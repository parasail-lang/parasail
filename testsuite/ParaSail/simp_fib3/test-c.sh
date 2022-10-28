# The locale specified by the environment affects sort order.
export Set LC_ALL=C
COMPILE="../../support/compile.sh"
 
$COMPILE simp_fib3.psl
./psl.exe 2>&1 > output
cat output | sort
rm output
