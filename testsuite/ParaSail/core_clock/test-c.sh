# The locale specified by the environment affects sort order
export Set LC_ALL=C
COMPILE="../../support/compile.sh"

$COMPILE call_aaa_test.psl
./psl.exe |
  sort |
  grep -v random                     |
  grep -v "About to delay"           |
  grep -v "Elapsed time"             |
  grep -v "In loop delaying for 1.0" |
  sed '/^$/d'                # Remove empty lines
  
