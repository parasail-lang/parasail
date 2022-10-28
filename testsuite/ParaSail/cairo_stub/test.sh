RUN="psli ../aaa.psi"

echo |
  $RUN cairo_stub.psl 2>&1 > output.txt

cat output.txt | grep -i "Error:"
cat output.txt | grep -i "Warning:"

rm output.txt
