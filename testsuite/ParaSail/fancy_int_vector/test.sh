RUN="psli ../aaa.psi"

echo |
  $RUN fancy_int_vector.psl |
  grep "Error:"
