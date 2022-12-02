RUN="psli ../aaa.psi"

echo |
  $RUN gtk_cairo.psl |
  grep "Error:"
