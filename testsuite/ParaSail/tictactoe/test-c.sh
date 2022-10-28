COMPILE="../../support/compile.sh"
 
$COMPILE tictactoe.psl
echo "X
1
1
O
1
2
Z
quit" |
  ./psl.exe
