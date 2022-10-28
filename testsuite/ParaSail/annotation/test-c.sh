COMPILE="../../support/compile.sh"

echo "*** Test 1: Annotations are OK"
$COMPILE annotation_ok.psl
./psl.exe

rm *.o *.s *.ll

echo "*** Test 2: One annotation failed (original test)"
$COMPILE annotation.psl
(./psl.exe 2>&1) |
  grep -i "Assertion failed"
