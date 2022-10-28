RUN="psli ../aaa.psi"

echo |
  $RUN hello_button.psl |
  grep "Error:"
