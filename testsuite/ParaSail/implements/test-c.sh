COMPILE="../../support/compile.sh"
 
$COMPILE implements.psl
(./psl.exe 2>&1) | grep -i "Error:"
