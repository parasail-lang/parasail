RUN="psli ../aaa.psi"

case $1 in
  1) $RUN array_sum.psl -command Array_Sum   # FAILED
     ;;
  2) $RUN array_sum.psl -command Array_Sum2
     ;;
  3) $RUN array_sum.psl -command Array_Sum3
     ;;
  4) $RUN array_sum.psl -command Array_Sum4
     ;;
  5) $RUN array_sum.psl -command Array_Sum5 100
     ;;

  *) $RUN array_sum.psl -command Array_Sum2
     $RUN array_sum.psl -command Array_Sum3
     $RUN array_sum.psl -command Array_Sum4
     $RUN array_sum.psl -command Array_Sum5 100
     ;;
esac
