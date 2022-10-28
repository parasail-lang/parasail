RUN="psli ../aaa.psi"

(echo | $RUN error_test.psl 2>&1) | grep "Error:"
