RUN="psli ../aaa.psi"

echo |
  $RUN measurement.psl |
  grep "Error:"
