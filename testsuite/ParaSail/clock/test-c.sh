COMPILE="../../support/compile.sh"
 
($COMPILE clock.psl 2>&1) |
  grep "clock.psl"
