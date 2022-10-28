#!/bin/bash
# Script written to repeatedly compile a ParaSail test
# Restriction: All the sources must be located in a single file
# ----------------------------------------------------------------------
# Author: Javier Miranda
#
# Use: do_test_single IntNum infile [outfile]
#
#   IntNum  Integer number which specifies how many times the test
#           must be repeated
#
#   infile  Text file containing the list of names of the tests.
#           Tests may be temporarily disabled by means of adding "#" at
#           the beginning of their name. For example:
#
#              annotation.psl
#              #array.psl       This test is silently skipped
#              bmap.psl
#
#   outfile Text file where the output of the failed and killed tests
#           is generated. If outfile is not specified then the output
#           is filed in "test.out"
#
# Example:
#   do_test_single 30 simp_loop.psl
#     Executes 30 times: "pslc aaa.psi simp_loop.psl"
#
# ----------------------------------------------------------------------------
# Internal files and variables

RUNTIME_PARASAIL="aaa.psi"
RUNTIME_SPARKEL="aaa.ski"

COMPILER_PARASAIL="pslc"
COMPILER_SPARKEL="sklc"

RUNTIME=""
COMPILER=""

INFILE=""                      # Input: (Default) list of test files
OUTFILE="test-single.out"      # Output: (default) output of failed tests
LOGFILE="test-single.log"

OUTALL="test-single_all.out"   # Internal: output of all tests
TMPOUT_1="tmp1.out"            # Internal: output of current test
TMPOUT_2="tmp2.out"            # Internal: errors of current test

NUM_TESTS=0
NUM_FAILED_TESTS=0
VERBOSE=0

# -----------------------------------------------------------------------------
# Process the command line

case "x$1" in
  "x") echo "Use: do_test_single IntNumber infile [outfile]"
       exit -1
       ;;

    *) NUM_TESTS=$1
       case "x$2" in
         "x") echo "Use: do_test_single IntNumber infile [outfile]"
              exit -1
              ;;

           *) INFILE=$2
              case "x$3" in
                "x") ;;
                  *) OUTFILE=$3
                     ;;
              esac
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
  *) echo "NUM_TESTS=$NUM_TESTS"
     echo "INFILE=$INFILE"
     echo "OUTFILE=$OUTFILE"
     echo "Press ENTER to continue"
     read letter
     ;;
esac

# -----------------------------------------------------------------------------
# Use the extension of the input file to select the compiler

echo $INFILE | grep "\.psl" > /dev/null
case $? in
  0) COMPILER=$COMPILER_PARASAIL
     RUNTIME=$RUNTIME_PARASAIL
     ;;

  1) echo $INFILE | grep "\.skl" > /dev/null
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
     exit -3
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

case $COMPILER in
  "sklc") trap 'echo ""; echo "Tests aborted"; PSLIST=`ps aux | grep -i sklc | grep -v grep | awk "{print $2}"`; kill -9 $PSLIST; exit 1' 2
          ;;
  "pslc") trap 'echo ""; echo "Tests aborted"; PSLIST=`ps aux | grep -i pslc | grep -v grep | awk "{print $2}"`; kill -9 $PSLIST; exit 1' 2
          ;;
esac


while test $NUM_TESTS -gt 0; do

   # ------------------- Execute one test
   echo -n "**** $INFILE "
   (echo "" | $COMPILER $RUNTIME $INFILE 2>&1) > $TMPOUT_1 &

   PID=$!

   sleep 1.5    # Maximim time for test
   # Currently set to 1'5 seconds since the tests are not being executed
   # (we are only compiling)

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

   # ------------------- Append output to the output of all tests
   # Used to supervise manually the output of the tests and tune
   # this script if some test is silently skipped!

   cat $TMPOUT_1 >> $OUTALL

   # ------------------- Search for errors in the output of the current test

   case $FINISHED in
      0) # The execution of the test had to be killed
         echo "(killed)"
         echo "**** $INFILE (killed)" >> $OUTFILE
         cat $TMPOUT_1 >> $OUTFILE
         let NUM_FAILED_TESTS='NUM_FAILED_TESTS + 1'
         ;;

      1) if test -f $TMPOUT_2; then
            rm $TMPOUT_2
         fi
         grep -i "Error:" $TMPOUT_1 > $TMPOUT_2

         if test -s $TMPOUT_2; then
            echo "(failed)"
            echo "**** $INFILE (FAILED)" >> $OUTFILE
            cat $TMPOUT_2  >> $OUTFILE
            let NUM_FAILED_TESTS='NUM_FAILED_TESTS + 1'
         else
            echo "(ok)"
         fi
         ;;
   esac

   let NUM_TESTS='NUM_TESTS - 1'
done

echo $NUM_FAILED_TESTS "failed tests"

case $NUM_FAILED_TESTS in
  0) ;;

  *) echo "detailed report available in file $OUTFILE"
     date >> $LOGFILE
     echo $NUM_FAILED_TESTS "failed tests" >> $LOGFILE
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

