#! /bin/tcsh

# compile ada202x files
# call with "-h" for help

# a2xc.csh must be in your path
set dir_called_from = $cwd
set script_loc = "$0"
set bin_dir = $script_loc:h

cd $bin_dir
set bin_dir = $cwd
cd $dir_called_from

set psl_dir = $bin_dir:h
if ("$psl_dir:t" == "install") then
   set psl_dir = $psl_dir:h
endif

# TODO: version
set version_string = "Ada202X LLVM-Based Compiler version 2.0"
# n = 5 compile the standard library in pieces (aaa/aaa*.ps?)

# Now the default:
#   echo "  -s         Try to use the stack for large local objects (beta)."

set exit_stat = 0

# Give help if no parameters
if ("$#argv" == 0) then
   echo $version_string
   goto print_help
   exit
endif

# a ':' following a letter means that letter takes an argument
set valid_flags = "o:O:cm:gikb:lvwdhtxspan:"
set argv = `getopt $valid_flags $*`
set exit_stat = "$status"
# TODO: full word arguments with --.
# get opt puts all the options before a -- and all the operands after

# if getopt failed then $status would be nonzero
# if getopt failed then we should fail as well
if ("$exit_stat" != 0) then
   echo $version_string
   goto print_help
endif

# default values:
# output executable name (will be "a2x.exe" if not otherwise set)
set exe = ""

# clang flags
set debug_asm = "-g"

# compiler Flags
set a2xc_flags = ""
set sequential = 0

# interpreter flags
set a2xi_flags = ""

# llc (ll -> .s) flags
set llc_flags = "-asm-verbose -O=0"

# whether or not to link with Ada
set link = 1

# interpret the compiler or use the compiled compiler
set use_interpreter = 0

# whether or not to delete .ll and .s
set delete_intermediate = 1

# bootstrap = 1 -> compile std library
# bootstrap = 2 -> compile the compiler
# bootstrap = 3 -> compile std library and compiler
set compile_parascope = 0

set bootstrap = 0
set verbose_flg = ""

# main_name = <name-of-main-routine> -> create file <main_name>__main.psl
set main_name = ""
set do_time = ""

set optim = 0
set parallelize_flag = 0
set explicit_par_only_flag = 0
set inlining = -1

# loop over all command line arguments
while ("$1" != "--")
   switch ($1)
      case "-o":
         # set output file name
         set exe = "$2"
         if ("$exe" !~ "/*") then
            # a relative path
            set exe = "$dir_called_from/$2"
         endif
         shift
         breaksw
      case "-O":
         # set optimization level
         set a2xc_flags = ($a2xc_flags "-O$2")
         set llc_flags = "-asm-verbose -O=$2"
         set optim = "$2"
         shift
         breaksw
      case "-c":
         # don't link
         set link = 0
         breaksw
      case "-m":
         # set name of main routine
         set main_name = "$2"
         shift
         breaksw
      case "-g":
         # set debug
         # TODO: debug levels
         set debug_asm = ""
         set a2xc_flags = ($a2xc_flags '"'"-g"'"')
         breaksw
      case "-i":
         # use the interpreted compiler
         # instead of the compiled compiler
         set use_interpreter = 1
         breaksw
      case "-k":
         # keep the .ll and the .s
         set delete_intermediate = 0
         breaksw
      case "-n":
         # control inlining
         set inlining = "$2"
         shift
         switch ($inlining)
           case "0":
             set a2xc_flags = ($a2xc_flags "--inline=0")
             breaksw
           case "1":
             set a2xc_flags = ($a2xc_flags "--inline=20")
             breaksw
           case "2":
             set a2xc_flags = ($a2xc_flags "--inline=30")
             breaksw
           default:
             echo "inlining flag -n$2 not recognized"
             set exit_stat = -1
             goto print_help
         endsw

         breaksw
      case "-b":
         # bootstrap the chunks and compiler itself
         set bootstrap = "$2"
         shift
         breaksw
      case "-a":
         # tell compiler to leave in assertions even when optimizing
         set a2xc_flags = ($a2xc_flags '"'"--checks"'"')
         breaksw
      case "-l":
         # tell compiler to run loop forward instead of concurrent
         set a2xc_flags = ($a2xc_flags '"'"--forward"'"')
         set sequential = 1
         breaksw
      case "-x":
         # tell front end to not insert implicit parallelism
         set a2xi_flags = ($a2xi_flags "-parcalls off")
         set explicit_par_only_flag = 1
         breaksw
      case "-p":
         # tell front end to not insert implicit parallelism
         set a2xi_flags = ($a2xi_flags "-parcalls on")
         set parallelize_flag = 1
         breaksw
      case "-w":
         # tell front end to dump ParaSail VM instructions
         set a2xi_flags = ($a2xi_flags "-listing on")
         breaksw
      case "-s":
         # try to use stack for large local objects (beta)
         set a2xc_flags = ($a2xc_flags "--stack")
         breaksw
      case "-t":
         # tell compiler to run loop forward instead of concurrent
         set do_time = "time "
         breaksw
      # TODO: case "--verbose":
      case "-v":
         # print version info and set verbose
         echo $version_string
         set verbose_flg = "--verbose"
         breaksw
      case "-d":
         echo $version_string
         set verbose_flg = "--debug"
         breaksw
      # TODO: case "--help":
      case "-h":
         # print help info and quit
         echo $version_string
         goto print_help
         exit
   endsw
   shift
end
shift # ignore the -- that separates arguments and files
# now, $* only contains files, not arguments

if (! -e $psl_dir/bin/a2x_compiler.exe) then
   set use_interpreter = 1
endif

if ("$parallelize_flag" == 0 && \
    "$explicit_par_only_flag" == 0) then
   if ("$optim" > 0 || "$use_interpreter" == 1) then
      #  When optimizing or using the interpreter, turn off automatic
      #  parallelization unless explicitly requested.
      #  NOTE: We turn it off when interpreting to reduce the stack usage
      #        within the interpreter.
      set a2xi_flags = ($a2xi_flags "-parcalls off")
   endif
endif

set a2xc_flags = ($a2xc_flags "$verbose_flg")

set comp_lib = ""

set standard_library = "$psl_dir/lib/aaa.a2i"
set parasail_std_lib = "$psl_dir/lib/aaa.psi"

if ($bootstrap % 2 == 1) then
   # compiling the standard library (bootstrap == 1 or == 3 or == 5)
   set comp_lib = ""
   set delete_intermediate = 0
else
   # not compiling std library
   set comp_lib = "$standard_library"
endif

set compiler_srcs = "-lang parasail $psl_dir/lib/aaa.psi"
set compiler_srcs = ($compiler_srcs $psl_dir/lib/reflection.ps?)
set compiler_srcs = ($compiler_srcs $psl_dir/lib/llvm_printer.ps?)
set compiler_srcs = ($compiler_srcs $psl_dir/lib/psvm_debugging.ps?)
set compiler_srcs = ($compiler_srcs $psl_dir/lib/type_desc_llvm_utils.ps?)
set compiler_srcs = ($compiler_srcs $psl_dir/lib/compiler.ps?)

set debugger_console = "$psl_dir/lib/debugger_console.psl"

set compiler_main = "$psl_dir/lib/compiler_main.psl"

set compiler_and_deps = "$compiler_srcs"

# depending on the level of bootstrapping,
# add standard library and/or compiler and its dependencies

# "files" are the filenames of the Ada 202X code that need to be
# parsed in addition to the sources for the compiler itself.
set files = ""

# "to_be_compiled" are the Ada 202x files for which we want to generate
# code
set to_be_compiled = ""
# "psl_to_be_compiled" are the ParaSail files for which we want to generate
# code
set psl_to_be_compiled = ""

set objs = ""
foreach i ($*)
   if ("$i:r.o" == "$i") then
      # already compiled
      if ("$i" =~ "/*") then
         set objs = ($objs "$i")
      else
         set objs = ($objs "$dir_called_from/$i")
      endif
   else
      if ("$i" =~ "-*") then
         echo "switch not understood: ${i}"
         set exit_stat = -1
         goto print_help
      endif

      set files = ($files "$i")
   endif
end

if ("$main_name" != "") then
   set main_file = "${main_name}__main.a2x"
   rm -rf $main_file

   echo "with $main_name;" > $main_file
   echo "procedure main(Args : Basic_Array<Univ_String>) is" >> $main_file

   if (`egrep -c "procedure ${main_name} *(is|;)" "$files"` == "0") then
      echo "   ${main_name}(Args);" >> $main_file
   else
      echo "   ${main_name};" >> $main_file
   endif

   echo "end main;" >> $main_file

   set files = ($files $main_file)
endif

set to_be_compiled = ($files)

set make_target = "compiled_a2x_main"

if ($bootstrap == 1) then
   # compile the a2x and parasail standard library sources
   set to_be_compiled = ($standard_library $files)
   set psl_to_be_compiled = ($parasail_std_lib)
   set link = 0
endif
if ($bootstrap == 2) then
   # compile the compiler sources
   set to_be_compiled = ($files)
   set psl_to_be_compiled = ($compiler_srcs $compiler_main)
   set make_target = "compiled_a2x_main_with_interp"
   set files = ($files -lang parasail $compiler_main)
endif
if ($bootstrap == 3) then
   # compile the standard library and compiler sources
   set to_be_compiled = ($standard_library $files)
   set psl_to_be_compiled = ($compiler_and_deps $compiler_main)
   set make_target = "compiled_a2x_main_with_interp"
   set files = ($files -lang parasail $compiler_main)
endif

if ("$exe" == "") then
   # this is the default executable
   set exe = $dir_called_from/a2x.exe
   if ($bootstrap == 2 || $bootstrap == 3) then
      # this is where the compiled-compiler resides
      set exe = $psl_dir/bin/a2x_compiler.exe
   endif
endif

# run the a2x compiler, if necessary
if ("$to_be_compiled$psl_to_be_compiled" != "") then
   if ($use_interpreter == 1) then
      if ("$verbose_flg" != "") then
         echo $do_time $psl_dir/build/bin/ada202x_main $a2xi_flags $standard_library -lang parasail $compiler_and_deps $debugger_console -lang ada202x $files -command Compile $a2xc_flags $to_be_compiled $psl_to_be_compiled
      endif
      $do_time $psl_dir/build/bin/ada202x_main $a2xi_flags $standard_library -lang parasail $compiler_and_deps $debugger_console -lang ada202x $files -command Compile $a2xc_flags $to_be_compiled $psl_to_be_compiled
      if ("$status" != 0) then
         exit "$status"
      endif
   else
      if ("$verbose_flg" != "") then
         echo $do_time $psl_dir/bin/a2x_compiler.exe $a2xi_flags $to_be_compiled -lang parasail $comp_lib $psl_to_be_compiled $psl_dir/lib/compiled_main.psl -command Compiled_Main $a2xc_flags $to_be_compiled $psl_to_be_compiled
      endif
      $do_time $psl_dir/bin/a2x_compiler.exe $a2xi_flags $to_be_compiled -lang parasail $comp_lib $psl_to_be_compiled $psl_dir/lib/compiled_main.psl -command Compiled_Main $a2xc_flags $to_be_compiled $psl_to_be_compiled
      if ("$status" != 0) then
         exit "$status"
      endif
   endif
endif

if ("$status" != 0) then
   exit "$status"
endif

# the compiler spits out *.a2x.ll files
# compile them down to '*.a2x.o's
foreach i ($to_be_compiled)
   if ($sequential == 1) then
      #  Be more verbose when compiling sequentially
      echo "   [llc $i.ll]"
   endif
   llc $llc_flags $i.ll
   if ("$status" != 0) then
      exit "$status"
   endif

   clang -c $debug_asm $i.s -o $i.o
   if ("$status" != 0) then
      exit "$status"
   endif

   # optionally delete intermediate representations
   if ($delete_intermediate == 1) then
      rm -f $i.ll
      if ("$debug_asm" == "") then
         # delete asm unless using it for debugging
         rm -f $i.s
      endif
   endif

   if ("$i" =~ "/*") then
      set objs = ($objs "$i.o")
   else
      set objs = ($objs "$dir_called_from/$i.o")
   endif
end

if ($link == 1 && "$to_be_compiled" != "") then
   # link objs together and with Ada
   if ($bootstrap != 3) then
      set objs = ($objs "$psl_dir/lib/aaa.a2i.o" "$psl_dir/lib/aaa.psi.o")
   endif

   if ($sequential == 1) then
      echo "   [build $make_target]"
      set silent = ""
   else
      set silent = "SILENT=1"
   endif

   cd $psl_dir; make --silent $make_target OBJS="$objs" A2X_EXE="$exe" $silent
endif

exit

print_help:
echo "valid flags are"
echo "  -o <file_name> Set executable output file name"
echo "  -On            Optimization level, where n is in 0 .. 3."
echo "  -a             Include assertions even when optimizing."
echo "  -n0            Turn off implicit inlining."
echo "  -n1            Turn on implicit inlining of small routines."
echo "  -n2            Turn on implicit inlining of larger routines."
echo "  -c             Compile only, do not link"
echo "                 If -c is *not* supplied, then -m *must* be supplied or"
echo "                 one of the files must contain a"
echo "                 procedure main [(Args : Basic_Array<Univ_String>)]"
echo "  -m <name>      Specify the name of the main routine which must"
echo "                 be declared procedure <name>(Basic_Array<Univ_String>)"
echo "                 or simply procedure <name>()"
echo "  -g             Generate A2X debug information."
echo "                 If -g is not supplied,"
echo "                 assembly debug information is generated"
echo "  -i             Use the interpreter to execute the compiler,"
echo "                 even if bin/a2x_compiler.exe exists."
echo "  -k             Keep .ll and .s intermediate representatations"
echo "  -bn            Where n is in 1 .. 4."
echo "                 n = 1 Compile the standard library (10 mins)."
echo "                 n = 2 Compile (i.e. bootstrap) the compiler (10 mins)."
echo "                 n = 3 Compile the standard library and bootstrap the compiler."
echo "                   This may take a long time -- about 20 cpu mins."
echo "  -l             Linear: do not compile files in parallel."
echo "  -x             eXplicit parallelism only: do not add implicit parallelism."
echo "  -p             Parallelize automatically certain out-of-line calls."
echo "  -t             Time: Display the execution time for the compiler."
echo "  -v             Print version and other verbose output."
echo "  -d             Print very verbose (debug) output."
echo "  -w             Print ParaSail virtual machine instructions."
echo "  -h             Print this help message"
echo "Before anything else, you must run: bin/a2xc.csh -b3"
echo "(it will take 20+ minutes of CPU time, about 6 mins on a 4-core machine)"

exit $exit_stat
