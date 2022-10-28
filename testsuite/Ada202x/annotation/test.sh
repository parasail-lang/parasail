RUN="a2xi ../aaa.a2i"

echo "*** Test 1: Annotations are OK"
$RUN annotation_ok.a2x -command Test_Annotations_Ok

echo "*** Test 2: One annotation failed (original test)"
($RUN annotation.a2x -command Test_Annotations 2>&1) |
 grep "Assertion failed"
