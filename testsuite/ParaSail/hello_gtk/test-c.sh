COMPILE="../../support/compile.sh"

$COMPILE gtk_cairo.psl hello_gtk.psl |
  grep "Error:"
