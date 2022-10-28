#!/bin/bash
#
# Author: Javier Miranda
# Initial version: August, 2014
#
# Description:
#   This script executes all the tests in the testsuite.
#   It relies on runtests.sh
#
# Requirement:
#   Same requirements of runtests.sh
# ------------------------------------------------------------------------


case `basename $PWD` in
  "testsuite") ;;
            *) echo "error: wrong directory"
               echo " This script must be executed in the testsuite directory"
               exit -1
               ;;
esac

BASEDIR=$PWD
for J in *; do
   if test -d $J; then
      case $J in
         "ParaSail" | \
         "Sparkel"  | \
         "Ada202x"  | \
         "Parython" | \
         "Javallel")
               echo $J
               cd $J
               rm -f runtests.out
               ../support/runtests.sh
               cd $BASEDIR
               ;;
      esac
   fi
done

find . -name runtests.out
