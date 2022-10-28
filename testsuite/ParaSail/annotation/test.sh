RUN="psli ../aaa.psi"

echo "*** Test 1: Annotations are OK"
$RUN annotation_ok.psl -command Test_Annotations

echo "*** Test 2: One annotation failed (original test)"
($RUN annotation.psl -command Test_Annotations 2>&1) |
 grep "Assertion failed"
