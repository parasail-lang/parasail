RUN="skli ../aaa.ski"

echo "*** Test 1: Annotations are OK"
$RUN annotation_ok.skl -command Test_Annotations

echo "*** Test 2: One annotation failed (original test)"
($RUN annotation.skl -command Test_Annotations 2>&1) |
 grep "Assertion failed"
