#! /bin/bash

# This is for bootstrapping the compiler
# First, create llvm files for the compiler and all of its dependencies
# Then, use this script with to link the back end with the front end
# to create a fully compiled compiler

exe="examples/compiler.exe"
if [ "$1" == "-o" ]
then
   exe="examples/$2"
   shift; shift
fi

# if -g is supplied, use psl debug info
# if not, use assembly debug info
debug_asm="-g"
if [ "$1" == "-g" ]
then
   debug_asm=""
   shift
fi

llc_flags="-asm-verbose -O0"
if [ "$1" == "-O1" ]
then
   llc_flags="-O1"
   shift
elif [ "$1" == "-O2" ]
then
   llc_flags="-O2"
   shift
elif [ "$1" == "-O3" ]
then
   llc_flags="-O3"
   shift
fi

objs=""

cd ..
for std_lib_piece in examples/aaa/aaa*.ps?.o
do
   objs="$objs $std_lib_piece"
done
cd examples

for i in "$@"
do
   llc $llc_flags $i.ll
   clang -c $debug_asm $i.s
   # rm -f $i.ll $i.s
   objs="$objs examples/$i.o"
done


cd ..; make compiled_main_with_interp OBJS="$objs" EXE="$exe"

