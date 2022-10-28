#! /bin/csh -fe
# Compile the rts files

# input aa*.ps? rts files
# output .ll, .s, and .o files

# supply -g if you want ParaSail debug information

# -O[123] is an optimization level and will be passed on to llc

# this is counter-intuitive: supplying -g turns OFF the -g flag to clang
# the reasoning is that -g means ParaSail debugging
# is on, and you don't want to generate assembly debugging info
set debug_asm = "-g"
if "$1" == "-g" then
   set debug_asm = ""
   shift
endif

set llc_flags = "-O0 -asm-verbose"
if "$1" == "-O1" then
   set llc_flags = "-O1"
   shift
else if "$1" == "-O2" then
   set llc_flags = "-O2"
   shift
else if "$1" == "-O3" then
   set llc_flags = "-O3"
   shift
endif

../../build/bin/parasail_main aaa*.ps? ../reflection.ps? ../llvm_printer.ps? ../compiler.ps? -command Compile $*

foreach i ($*)
   llc $llc_flags $i.ll
   clang -c $debug_asm $i.s
   #rm -f $i.s
end
