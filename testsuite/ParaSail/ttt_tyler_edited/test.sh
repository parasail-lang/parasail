RUN="psli ../aaa.psi"

# Execution disabled
# $RUN ttt_tyler_edited.psl -command start

# For now we just compile this test
echo |
  $RUN ttt_tyler_edited.psl |
  grep "Error:"
