RUN="psli ../aaa.psi"

echo | $RUN const.psl | grep "Error:"
