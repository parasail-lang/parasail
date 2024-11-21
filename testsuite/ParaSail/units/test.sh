RUN="psli ../aaa.psi"

echo "--- units.psl ---"
echo ""
$RUN units.psl -command Test_Units
echo ""
echo "--- bad_units.psl ---"
echo ""
$RUN units.psl bad_units.psl -command Test_Units
