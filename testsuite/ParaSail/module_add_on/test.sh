RUN="psli ../aaa.psi"

echo |
 $RUN module_add_on.psl |
 grep "Error:"
