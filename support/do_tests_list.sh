#!/bin/bash
# Script written to compile tests composed of a single file
# Author: Javier Miranda

# Use: do_test_list [infile [outfile]]
#
#   infile  Text file containing the list of names of the tests.
#           Tests may be temporarily disabled by means of adding "#" at
#           the beginning of their name. For example:
#
#              annotation.psl
#              #array.psl       This test is silently skipped
#              bmap.psl
#
#           If infile is not specified then the input file is test_list.txt
#
#   outfile Text file where the output of the failed and killed tests
#           is generated. If outfile is not specified then the output
#           is filed in "test.out"
#
# -----------------------------------------------------------------------------
# Internal files and variables

RUNTIME_PARASAIL="aaa.psi"
RUNTIME_SPARKEL="aaa.ski"

COMPILER_PARASAIL="pslc"
COMPILER_SPARKEL="sklc"

RUNTIME=""
COMPILER=""

INFILE="test_list.txt"       # Input: (Default) list of test files
OUTFILE="test.out"           # Output: (default) output of failed tests
LOGFILE="test.log"

OUTALL="test_all.out"        # Internal: output of all tests

TMPOUT_1="tmp1.out"          # Internal: output of current test
TMPOUT_2="tmp2.out"          # Internal: errors of current test

NUM_FAILED_TESTS=0
VERBOSE=0
DELAY=0                      # Internal: Maximum time for test (in seconds)
NUM_TESTS=0                  # Internal: Tests counter

# -----------------------------------------------------------------------------
# Process the command line

case "x$1" in
  "x") ;;

    *) INFILE=$1
       case "x$2" in
         "x") ;;
           *) INFILE=$2
              ;;
       esac
       ;;
esac

if ! test -f $INFILE; then
   echo "error: input file not found ($INFILE)"
   exit -2
fi

case $VERBOSE in
  0) ;;
  *) echo "INFILE=$INFILE"
     echo "OUTFILE=$OUTFILE"
     echo "Press ENTER to continue"
     read letter
     ;;
esac

# -----------------------------------------------------------------------------
# Use the extension of the first input file to select the compiler

LIST=`cat $INFILE | grep -v ^#`
for J in $LIST; do
   FIRST_FILE=$J
   break
done

echo $FIRST_FILE | grep "\.psl" > /dev/null
case $? in
  0) COMPILER=$COMPILER_PARASAIL
     RUNTIME=$RUNTIME_PARASAIL
     ;;

  1) echo $FIRST_FILE | grep "\.skl" > /dev/null
     case $? in
        0) COMPILER=$COMPILER_SPARKEL
           RUNTIME=$RUNTIME_SPARKEL
           ;;
        1) echo "error: unknown file extension"
           exit -2
           ;;
     esac
     ;;
esac

# -----------------------------------------------------------------------------
# Verify that the compiler is available in the PATH

type $COMPILER > /dev/null
case $? in
  0) ;;
  1) echo "error: $COMPILER not available in PATH"
     exit -1
     ;;
esac

# -----------------------------------------------------------------------------
# clean the environment

for j in $OUTFILE $OUTALL $TMPOUT_1 $TMPOUT_2 ; do
   if test -f $j; then
      case $VERBOSE in
        1) rm -v $j
           ;;
        0) rm $j
           ;;
      esac
   fi
done

# ----------------------------------------------------------
# Execute the tests

trap 'echo ""; echo "Tests aborted"; exit 1' 2

LIST=`cat $INFILE | grep -v ^#`
for j in $LIST; do

   # ------------------- Execute one test
   echo -n "**** $j "

   case $DELAY in
      0) (echo "" | $COMPILER $RUNTIME $j 2>&1) > $TMPOUT_1;
         FINISHED=1
         ;;

      *) (echo "" | $COMPILER $RUNTIME $j 2>&1) > $TMPOUT_1 &
         PID=$!
         sleep $DELAY    # Maximim time for test

         # kill the execution of the test if not finished
         ps -p $PID 2>&1 > /dev/null
         FINISHED=$?

         case $FINISHED in
             0) # Not finished: killing process
                ps -p $PID > /dev/null
                kill -9 $PID 2>&1 > /dev/null
                ;;

             1) # OK: PID Not found
                ;;
         esac
         ;;
   esac

   # ------------------- Append output to the output of all tests
   # Used to supervise manually the output of the tests and tune
   # this script if some test is silently skipped!

   cat $TMPOUT_1 >> $OUTALL

   # ------------------- Search for errors in the output of the current test

   case $FINISHED in
      0) # The execution of the test had to be killed
         echo "(killed)"
         echo "**** $j (killed)" >> $OUTFILE
         cat $TMPOUT_1 >> $OUTFILE
         let NUM_FAILED_TESTS='NUM_FAILED_TESTS + 1'
         ;;

      1) if test -f $TMPOUT_2; then
            rm $TMPOUT_2
         fi
         grep -i "Error:" $TMPOUT_1 > $TMPOUT_2

         if test -s $TMPOUT_2; then
            echo "(failed)"
            echo "**** $j (FAILED)" >> $OUTFILE
            cat $TMPOUT_2  >> $OUTFILE
            let NUM_FAILED_TESTS='NUM_FAILED_TESTS + 1'
         else
            echo "(ok)"
         fi
         ;;
   esac

   let NUM_TESTS='NUM_TESTS + 1'
done

echo $NUM_TESTS "tests executed"
echo $NUM_FAILED_TESTS "failed tests"

case $NUM_FAILED_TESTS in
  0) ;;
  *) grep -i "\*\*\*\* " $OUTFILE
     echo "detailed report available in file $OUTFILE"

     date >> $LOGFILE
     grep -i "\*\*\*\* " $OUTFILE >> $LOGFILE
     echo "" >> $LOGFILE
     ;;
esac

for j in $TMPOUT_1 $TMPOUT_2; do
   if test -f $j; then
      case $VERBOSE in
        1) rm -v $j
           ;;
        0) rm $j
           ;;
      esac
   fi
done

