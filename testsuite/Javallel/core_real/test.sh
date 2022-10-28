RUN="jlli ../aaa.jli"

$RUN -command testReal 2> errors > output

cat output
grep "Error:" errors

rm errors
rm output

