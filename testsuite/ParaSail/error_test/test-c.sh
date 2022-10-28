COMPILE="../../support/compile.sh"

($COMPILE error_test.psl 2>&1) | grep "Error:"
