RUN="psli ../aaa.psi"

# $RUN race_cond.psl -command Test_Race_Cond
echo | $RUN race_cond.psl | grep "Warning:"
