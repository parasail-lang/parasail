# The locale specified by the environment affects sort order.
export Set LC_ALL=C
COMPILE="../../support/compile.sh"
 
$COMPILE parallel_stmt.psl
./psl.exe |
  sort |
  awk '/2673168/ {print "168"
                  print "2673"
                  next}
                 {print}' |
  sed '/^$/d'                # Remove empty lines
