RUN="psli ../aaa.psi"

echo |
  $RUN extend_errs.psl |
  grep "Error:"
