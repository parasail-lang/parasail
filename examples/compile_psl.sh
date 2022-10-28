#! /bin/bash
# Compile .psl files to .psl.o files
# then link them in with the ParaSail Standard Library
# and produce an executable "examples/psl.exe" by default.

# if -o is the first flag, then following file name will
#   be the output executable

# next may come -O or -g (not both yet)

# -g creates ParaSail debug information (that's kinda lousy at the moment)
# if -g is not supplied, debug information will point to the assembly file

# -O performs a small optimization on if statements with integer or
# float comparisons

exe="examples/psl.exe"
if [ "$1" == "-o" ]
then
   exe="examples/$2"
   shift; shift
fi

# if -g is supplied, use psl debug info
# if not, use assembly debug info
debug_asm="-g"
psl_flags=""
if [ "$1" == "-g" ]
then
   debug_asm=""
   psl_flags="\"-g\""
   shift
elif [ "$1" == "-O" ]
then
   # -O and -g together is not allowed
   psl_flags="\"-O\""
   shift
fi

../build/bin/parasail_main aaa/aaa*.ps? reflection.ps? llvm_printer.ps? compiler.ps? "$@" -command Compile $psl_flags "$@"

objs=""

cd ..
for std_lib_piece in examples/aaa/aaa*.ps?.o
do
   objs="$objs $std_lib_piece"
done
cd examples

for i in "$@"
do
   llc -asm-verbose -O0 $i.ll
   clang -c $debug_asm $i.s
   # rm -f $i.ll $i.s
   objs="$objs examples/$i.o"
done


cd ..; make compiled_main OBJS="$objs" EXE="$exe"

