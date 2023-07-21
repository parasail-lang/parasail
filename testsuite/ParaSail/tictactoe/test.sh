RUN="psli ../aaa.psi"

echo "X
1
1
O
1
2
Z
quit" |
  $RUN tictactoe.psl -command Play 5
