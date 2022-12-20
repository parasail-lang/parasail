RUN="psli ../aaa.psi"

($RUN clock.psl -command main 2>&1) |
 grep "clock.psl"

