COMPILE="../../support/compile.sh"
 
$COMPILE race_cond.psl
./psl.exe | grep "Warning:"
