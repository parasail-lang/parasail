# The locale specified by the environment affects sort order.
export Set LC_ALL=C
RUN="psli ../aaa.psi"

$RUN parallel_stmt.psl -command Test1 2 4 |
  sort |
  awk '/2673168/ {print "168"
                  print "2673"
                  next}
                 {print}' |
  sed '/^$/d'                # Remove empty lines
