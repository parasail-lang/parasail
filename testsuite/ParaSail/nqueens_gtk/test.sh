RUN="psli ../aaa.psi"

echo |
  $RUN gtk_cairo.psl nqueens_gtk.psl |
  grep "Error:"
