RUN="psli ../aaa.psi"

echo |
  $RUN gtk_cairo.psl hello_gtk.psl |
  grep "Error:"
