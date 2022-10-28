COMPILE="../../support/compile.sh"

$COMPILE gtk_cairo.psl nqueens_gtk.psl |
  grep "Error:"
