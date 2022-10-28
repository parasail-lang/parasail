RUN="psli ../aaa.psi"

echo "ParaSail" | $RUN call_aaa_test.psl -command main > output 2> errors

cat output
cat errors

rm output
rm errors

