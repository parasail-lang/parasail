RUN="psli ../aaa.psi"

(echo | $RUN implements.psl 2>&1) | grep -i "Error:"
