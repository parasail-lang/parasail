#!/bin/bash

# This script configures the runtime to use generic locks implemented by
# means of protected types or OMP locks.
#
#    Warning: This script is invoked from Makefile
#
#  $1 = "on"  Enables the OMP lib
#  $1 = "off" Enables the PO lib

if ! test -L psc-interpreter-locks.ads; then
   case $1 in
    "on")
        cp -p psc-interpreter-locks-omp.ads psc-interpreter-locks.ads
        cp -p psc-interpreter-locks-omp.adb psc-interpreter-locks.adb
        echo "Config: OMP locks enabled"
        ;;

     *) cp -p psc-interpreter-locks-po.ads psc-interpreter-locks.ads
        cp -p psc-interpreter-locks-po.adb psc-interpreter-locks.adb
        echo "Config: PO locks enabled"
        ;;
   esac
fi
