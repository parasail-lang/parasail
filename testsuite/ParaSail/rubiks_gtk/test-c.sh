COMPILE="../../support/compile.sh"

$COMPILE gtk_cairo.psl rubiks_gtk.psl |
  grep "Error:"
