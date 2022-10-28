RUN="psli ../aaa.psi"

echo |
  $RUN gtk_cairo.psl rubiks_gtk.psl |
  grep "Error:"
