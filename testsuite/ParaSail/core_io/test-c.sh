COMPILE="../../support/compile.sh"

$COMPILE call_aaa_test.psl
echo "ParaSail" | ./psl.exe > output 2> errors

cat output
cat errors

rm output
rm errors
