# The locale specified by the environment affects sort order
export Set LC_ALL=C
RUN="psli ../aaa.psi"

$RUN call_aaa_test.psl -command main |
  sort |
  grep -v random                     |
  grep -v "About to delay"           |
  grep -v "Elapsed time"             |
  grep -v "In loop delaying for 1.0" |
  sed '/^$/d'                # Remove empty lines
