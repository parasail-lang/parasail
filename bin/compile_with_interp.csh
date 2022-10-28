#! /bin/csh -fe
# Compile .psl files and link with ParaSail front end.
set exe = "examples/psl.exe"
if "$1" == "-o" then
   set exe = "examples/$2"
   shift; shift
endif

set srcs = ""
set to_be_comp = ""
set flags = ""
set nonomatch
foreach i ($*)
   if "$i:r" == "reflection" then
      # only compile
      set to_be_comp = ($to_be_comp $i)
   else if "$i:r" == "llvm_printer" then
      # only compile
      set to_be_comp = ($to_be_comp $i)
   else if "$i:r" == "compiler" then
      # only compile
      set to_be_comp = ($to_be_comp $i)
   else if "$i:r.psl" == "$i" then
      set srcs = ($srcs $i)
      set to_be_comp = ($to_be_comp $i)
   else if "$i:r.psi" == "$i" then
      set srcs = ($srcs $i)
      set to_be_comp = ($to_be_comp $i)
   else if "$i:r.o" != "$i" then
      set flags = ($flags $i)
   endif
end

if "$to_be_comp" != "" then
   # compile sources, if any
   echo ../build/bin/parasail_main $flags aaa/aaa*.ps? reflection.ps? \
      llvm_printer.ps? compiler.ps? $srcs -command Compile $to_be_comp
   ../build/bin/parasail_main $flags aaa/aaa*.ps? reflection.ps? \
      llvm_printer.ps? compiler.ps? $srcs -command Compile $to_be_comp
endif

set objs = ""
foreach i (aaa/aaa*.ps?.o $*)
   if "$i:r.o" == "$i" then
      #  already compiled
      if "$i" != "aaa/aaa*.ps?.o" then
         set objs = ($objs examples/$i)
      endif
   else if "$i:r.psl" == "$i" then
      #  finish the compilation job
      echo llc -asm-verbose -O0 $i.ll
      echo clang -c -g $i.s
      llc -asm-verbose -O0 $i.ll
      clang -c -g $i.s
      # rm -f $i.ll $i.s
      set objs = ($objs examples/$i.o)
   else if "$i:r.psi" == "$i" then
      #  finish the compilation job
      echo llc -asm-verbose -O0 $i.ll
      echo clang -c -g $i.s
      llc -asm-verbose -O0 $i.ll
      clang -c -g $i.s
      # rm -f $i.ll $i.s
      set objs = ($objs examples/$i.o)
   endif
end
cd ..; make compiled_main_with_interp OBJS="$objs" EXE="$exe"
