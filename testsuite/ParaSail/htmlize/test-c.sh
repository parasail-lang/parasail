COMPILE="../../support/compile.sh"
 
$COMPILE htmlize.psl
echo "ParaSail
test" | ./psl.exe
