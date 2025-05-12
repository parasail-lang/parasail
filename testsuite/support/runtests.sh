#!/bin/bash
#
# Author: Javier Miranda
# Initial version: July, 2014
#
# Description:
#   This script is used to emulate the basic functionality of the
#   mailserver. It assumes that no test hangs forever!
#
# Requirement:
#   ParaSail must be available in the PATH
# ------------------------------------------------------------------------

if [ "$DYLD_LIBRARY_PATH" = "" ]; then
   # Overcome MacOSX SIP which nulls out DYLD_* environment variables
   if test -f ../support/dyld_paths.source; then
      source ../support/dyld_paths.source
   fi
fi

LANGUAGE=`basename $PWD`

OUT_FILE=$PWD/runtests.out
PASSED_TNS_FILE=$PWD/runtests.passed

ERR_FILE=$PWD/runtests.errors
LOG_FILE=$PWD/runtests.log

NUM_TESTS=0
NUM_FAILED_TESTS=0
TOTAL_TESTS=0

case $1 in
  "-c") USE_COMPILER="yes"
        shift
        ;;
     *) USE_COMPILER="no"
        ;;
esac

trap 'echo ""; echo "Program aborted"; exit 1' 2

# ---------------------------------------------------------------------------
do_output ()
  # $1 = NUM_TESTS
  # $2 = NUM_FAILED_TESTS
  # $3 = test name
{
  echo -n -e "Num_Tests = $1/$TOTAL_TESTS (Failed = $2) $3    \r"
}

# ---------------------------------------------------------------------------
mainloop() {
   echo "Tests summary"
   echo "-------------"
   do_output $NUM_TESTS $NUM_FAILED_TESTS ""

   for i in $DIRS; do
       if test -d $i; then

          case $i in
            "support");;
            "tmp.rts");;
                "tmp");;

                *) # echo -n -e " " $i "\r"
                   do_output $NUM_TESTS $NUM_FAILED_TESTS $i

                   # Copy the contents of the TN into the temporary directory
                   # to ensure that the test is executed in a clean environment

                   rm -rf tmp/*
                   cp -r $i/* tmp

                   # Enter into the temporary directory to check this TN
                   cd tmp
                   TMP_DIR=$PWD

                   # Init script filenames

                   case $USE_COMPILER in
                     "yes") cmd="test-c.sh"
                            out="test-c.out"
                            if ! test -f $out; then
                               out="test.out"
                            fi
                            ;;
                      "no") cmd="test.sh"
                            out="test.out"
                            ;;
                   esac

                   # Handle test.opt

                   SKIP_TARGET="no"
                   if test -f test.opt; then
                      OPT_TARGET=`cat test.opt \
                                    | grep -iw skip  \
                                    | awk '{print $1}'`
                      case $OPT_TARGET in
                             "ALL") SKIP_TARGET="yes";;
                             "all") SKIP_TARGET="yes";;

                        "COMPILER") SKIP_TARGET=$USE_COMPILER;;
                        "compiler") SKIP_TARGET=$USE_COMPILER;;

                                 *) # Unsupported target
                                    echo -n $i " wrong target " >> $LOG_FILE
                                    echo -n "in test.opt " >> $LOG_FILE
                                    echo "($SKIP_TARGET)"  >> $LOG_FILE
                                    ;;
                      esac
                   fi

                   if [ "$SKIP_TARGET" = "yes" ]; then
                      echo $i " SKIPPED"      >> $LOG_FILE

                   elif ! test -f $cmd; then
                      echo $i " " $cmd " " $out " ($cmd not found)">> $ERR_FILE

                   else
                      echo $i " " $cmd " " $out " " >> $LOG_FILE

                      (source $cmd 2>&1) | \
                        egrep -v "^(ld:|Installing|object file)" > tmp.out

                      if test -f $out; then
                         diff -B -w -u $out tmp.out > diff.out
                         case $? in
                            0) echo $i >> $PASSED_TNS_FILE;;
                            *) echo " ************ TEST $i" >> $OUT_FILE
                               cat diff.out                 >> $OUT_FILE
               		          let NUM_FAILED_TESTS='NUM_FAILED_TESTS + 1'
                               ;;
                         esac
                      else
                         if test -s tmp.out; then
            	             echo " ************ TEST $i" >> $OUT_FILE
                            cat tmp.out                  >> $OUT_FILE
	                         let NUM_FAILED_TESTS='NUM_FAILED_TESTS + 1'
                         else
                            echo $i >> $PASSED_TNS_FILE
                         fi
                      fi

                      let NUM_TESTS='NUM_TESTS + 1'
                   fi

                   cd ..
                   ;;
          esac

       fi

       do_output $NUM_TESTS $NUM_FAILED_TESTS "                                "
   done

   echo ""
}

# ---------------------------------------------------------------------------
LIB_DIR="../../lib"
RTS_DIR="tmp.rts"

compile_rts_library() {
   if ! test -d $LIB_DIR; then
      echo "error: directory $LIB_DIR not found"
      exit -1
   fi

   # ------ Ensure that the runtime library is compiled

   tcsh ../../bin/pslc.csh -b1 -v

}

# ***************** Check if $OUT_FILE is present

if test -f $OUT_FILE; then
   echo -n "Remove $OUT_FILE (y/n)? "
   read Letter
   case $Letter in
     "y"|"Y") echo "Removing $OUT_FILE..."
              rm -f $OUT_FILE
              ;;
           *) echo "Execution cancelled"
              exit -1
              ;;
   esac
fi

# Check the availability of the temporary directory. Used to do a clean
# compilation of each test.

if ! test -d tmp; then
   mkdir -v tmp
fi

# Check the availability of the link to the ParaSail runtime library

case $LANGUAGE in
   "ParaSail") if ! test -L aaa.psi; then
                  ln -s ../../lib/aaa.psi
                  ln -s ../../lib/reflection.psi
                  ln -s ../../lib/reflection.psl
               fi
               ;;
    "Sparkel") if ! test -L aaa.ski; then
                  ln -s ../../sparkel_examples/aaa.ski
               fi
               ;;
    "Ada202x") if ! test -L aaa.a2i; then
                  ln -s ../../ada202x_examples/aaa.a2i
               fi
               ;;
   "Parython") if ! test -L aaa.pri; then
                  ln -s ../../parython_examples/aaa.pri
               fi
               ;;
   "Javallel") if ! test -L aaa.jli; then
                  ln -s ../../javallel_examples/aaa.jli
               fi
               ;;
            *) echo "error: unsupported language ($LANGUAGE)"
               echo "Warning: Most probably this script was executed"
               echo "  It must be executed in one of the following directories:"
               echo "   - testsuite/ParaSail"
               echo "   - testsuite/Sparkel"
               echo "   - testsuite/Ada202x"
               echo "   - testsuite/Parython"
               echo "   - testsuite/Javallel"
               exit -1
               ;;
esac

# ***************** Remove temporary files and clean the tmp directory

rm -f tmp/*
rm -f $PASSED_TNS_FILE
rm -f $ERR_FILE
rm -f $LOG_FILE

# ***************** Handle tests specified in the command line

case $* in
    "") DIRS="*";; # a) all the tests
     *) DIRS=$*;;  # b) tests specified in the command line
esac

AUX=""
for j in $DIRS; do
   if test -d $j; then
      case $j in
        "support") ;;
        "tmp.rts") ;;
            "tmp") ;;
                *) let TOTAL_TESTS='TOTAL_TESTS + 1'
                   AUX="$AUX $j"
                   ;;
      esac
   fi
done
DIRS=$AUX

echo $0         >> $LOG_FILE
echo `uname -n` >> $LOG_FILE

# ***************** Display the interpreter used by the script

clear

case $USE_COMPILER in
  "yes") case $LANGUAGE in
           "ParaSail") echo "*** ParaSail Testsuite"
                       echo "Using the ParaSail Compiler"
                       ;;
            "Sparkel") echo "error: compiler for Sparkel not available"
                       ;;
            "Ada202x") echo "error: compiler for Ada202x not available"
                       ;;
           "Parython") echo "error: compiler for Parython not available"
                       ;;
           "Javallel") echo "error: compiler for Javallel not available"
                       ;;
         esac

         if ! test -f $LIB_DIR/aaa.psi.o; then
            compile_rts_library

            # Handle errors (if any!)
            case $? in
              0) ;;
              *) exit -1;
            esac
         fi
         ;;

      *) case $LANGUAGE in
           "ParaSail") echo "*** ParaSail Testsuite"
                       TOOL="psli"
                       ;;
            "Sparkel") echo "*** Sparkel Testsuite"
                       TOOL="skli"
                       ;;
            "Ada202x") echo "*** Ada202x Testsuite"
                       TOOL="a2xi"
                       ;;
           "Parython") echo "*** Parython Testsuite"
                       TOOL="pryi"
                       ;;
           "Javallel") echo "*** Javallel Testsuite"
                       TOOL="jlli"
                       ;;
         esac

         TOOL_DIR=`(type $TOOL 2>&1) | cut -c9-`
         echo $TOOL_DIR | grep "not found" > /dev/null
         case $? in
           0) echo "error: $TOOL not available in PATH"
              echo "The current value of PATH is"
              echo "PATH=$PATH"
              exit -1
              ;;
           *) echo "Using $TOOL_DIR"
              ;;
         esac
         ;;
esac

# ***************** Run the tests
time mainloop

# ***************** Show location of reports and exit
echo ""
echo ""
if test -f $OUT_FILE; then
   echo "output saved in $OUT_FILE"
fi
echo ""

if test -f $ERR_FILE; then
   cat $ERR_FILE
   echo ""
   echo "these errors have been saved in $ERR_FILE"
fi

if test -f $OUT_FILE; then
   TNS=`cat $OUT_FILE | awk '/^ \*\*\*/ {print $3}'`
   echo "Failed:" $TNS
fi

