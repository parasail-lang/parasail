RUN="psli ../aaa.psi"

$RUN call_aaa_test.psl -command main 2> errors > output
cat output
grep "Error:" errors |
  awk '{for (j=2; j<=NF; j++)
          printf ("%s ", $j)
        print ("\n")}
      '

#rm errors
#rm output
