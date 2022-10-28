RUN="psli ../aaa.psi"

($RUN clock.psl -command Test_Time 3.0 2>&1) |
 grep "clock.psl"
