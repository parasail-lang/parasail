#! /bin/tcsh -e

# interpret Ada2X files
# call with "-h" for help

# ada_interp.csh must be in your path
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
set version_string = "Ada2X Interpreter version 9.0"
alias print_help eval '\
echo "ada_interp.csh [<flags>] [<lib1>.a2? ...] <main>.a2x [-command <op> <param> ...]"\
echo " Valid flags are:"\
echo "  -t             Time: Display the execution time for the interpreter."\
echo "  -x             eXplicit parallelism only: do not add implicit parallelism."\
echo "  -p             Parallelize automatically certain out-of-line calls."\
echo "  -v             Print version and other verbose output."\
echo "  -d             Print very verbose (debug) output."\
echo "  -w             Print ParaSail virtual machine instructions."\
echo "  -h             Print this help message"\
echo "  -command <operation> <param1> ..."\
echo "                 Execute the given operation with the given parameters"\
echo "                 and then exit (must appear last in command line)"\
'

# Give help if no parameters
if ("$#argv" == 0) then
   echo $version_string
   print_help
   exit
endif

# a ':' following a letter means that letter takes an argument
set valid_flags = "txpvdwh"
set argv = `getopt $valid_flags $*`
# TODO: full word arguments with --.
# get opt puts all the options before a -- and all the operands after

# if getopt failed then $status would be nonzero
# if getopt failed then we should fail as well
set exit_stat = "$status"
if ("$exit_stat" != 0) then
   echo $version_string
   print_help
   exit "$exit_stat"
endif

# default values:

# Ada2X interpreter flags
set psli_flags = ""

set verbose_flg = 0

set do_time = ""

set parallelize_flag = 0
set explicit_par_only_flag = 0

# loop over all command line arguments
while ("$1" != "--")
   switch ($1)
      case "-x":
         # tell Ada2X front end to not insert implicit parallelism
         set psli_flags = ($psli_flags "-parcalls off")
         set explicit_par_only_flag = 1
         breaksw
      case "-p":
         # tell Ada2X front end to not insert implicit parallelism
         set psli_flags = ($psli_flags "-parcalls on")
         set parallelize_flag = 1
         breaksw
      case "-w":
         # tell Ada2X front end to dump ParaSail VM instructions
         set psli_flags = ($psli_flags "-listing on")
         breaksw
      case "-t":
         # tell Ada2X interpreter to record time
         set do_time = "time "
         breaksw
      case "-v":
         # print version info and set verbose
         echo $version_string
         set verbose_flg = 1
         breaksw
      case "-d":
         echo $version_string
         set psli_flags = ($psli_flags "-debug on")
         set verbose_flg = 1
         breaksw
      # TODO: case "--help":
      case "-h":
         # print help info and quit
         echo $version_string
         print_help
         exit
   endsw
   shift
end
shift # ignore the -- that separates arguments and files
# now, $* only contains files, not arguments

if ("$parallelize_flag" == 0 && "$explicit_par_only_flag" == 0) then
   #  turn off automatic parallelization unless
   #  explicitly requested.
   set psli_flags = ($psli_flags "-parcalls off")
endif

set standard_library = "$psl_dir/lib/aaa.a2i"

set debugger_srcs = "-lang parasail $psl_dir/lib/aaa.psi"
set debugger_srcs = ($debugger_srcs $psl_dir/lib/reflection.ps?)
set debugger_srcs = ($debugger_srcs $psl_dir/lib/psvm_debugging.ps?)
set debugger_srcs = ($debugger_srcs $psl_dir/lib/debugger_console.psl)

if ($verbose_flg) then
   echo $do_time $psl_dir/build/bin/ada202x_main $psli_flags $standard_library $debugger_srcs -lang ada202x $*
endif
$do_time $psl_dir/build/bin/ada202x_main $psli_flags $standard_library $debugger_srcs -lang ada202x $*
