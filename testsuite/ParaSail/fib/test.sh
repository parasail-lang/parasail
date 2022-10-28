# The locale specified by the environment affects sort order.
export Set LC_ALL=C
RUN="psli ../aaa.psi"

$RUN fib.psl -command Test_Print 5 > output1
$RUN fib.psl -command Fibit 5      > output2
$RUN fib.psl -command Fib_Slow 5   > output3
$RUN fib.psl -command main         > output4
$RUN fib.psl -command main "\"10\"" > output5

cat output1
cat output2
cat output3 | sort
cat output4
cat output5

rm output1
rm output2
rm output3
rm output4
rm output5
