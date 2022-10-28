#! /bin/tcsh -e

# interpret sparkel files
# call with "-h" for help

# skl_interp.csh must be in your path
set dir_called_from = $cwd
set script_loc = "$0"
set bin_dir = $script_loc:h

cd $bin_dir
set bin_dir = $cwd
cd $dir_called_from

set skl_dir = $bin_dir:h
if ("$skl_dir:t" == "install") then
   set skl_dir = $skl_dir:h
endif

# TODO: version
set version_string = "Sparkel Interpreter version 8.0"
alias print_help eval '\
echo "skl_interp.csh [<flags>] [<lib1>.sk? ...] <main>.skl [-command <op> <param> ...]"\
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
if ("$#" == 0) then
   echo $version_string
   print_help
   exit
endif

# a ':' following a letter means that letter takes an argument
set valid_flags = "txpvdwh"
set argv = `getopt $valid_flags $*`
# TODO: full word arguments with --.
# get opt puts all the options before a -- and all the operands after

# if getopt failed then $? would be nonzero
# if getopt failed then we should fail as well
if ("$?" != 0) then
   echo $version_string
   print_help
   exit "$?"
endif

# default values:

# Sparkel interpreter flags
set skli_flags = ""

set verbose_flg = 0

set do_time = ""

set parallelize_flag = 0
set explicit_par_only_flag = 0

# loop over all command line arguments
while ("$1" != "--")
   switch ($1)
      case "-x":
         # tell sparkel front end to not insert implicit parallelism
         set skli_flags = ($skli_flags "-parcalls off")
         set explicit_par_only_flag = 1
         breaksw
      case "-p":
         # tell sparkel front end to not insert implicit parallelism
         set skli_flags = ($skli_flags "-parcalls on")
         set parallelize_flag = 1
         breaksw
      case "-w":
         # tell sparkel front end to dump ParaSail VM instructions
         set skli_flags = ($skli_flags "-listing on")
         breaksw
      case "-t":
         # tell sparkel interpreter to record time
         set do_time = "time "
         breaksw
      case "-v":
         # print version info and set verbose
         echo $version_string
         set verbose_flg = 1
         breaksw
      case "-d":
         echo $version_string
         set skli_flags = ($skli_flags "-debug on")
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
   set skli_flags = ($skli_flags "-parcalls off")
endif

set standard_library = "$skl_dir/sparkel_examples/aaa.ski"

set debugger_srcs = ""
# set debugger_srcs = "$psl_dir/lib/reflection.ps?"
# set debugger_srcs = ($debugger_srcs $psl_dir/lib/psvm_debugging.ps?)
# set debugger_srcs = ($debugger_srcs $psl_dir/lib/debugger_console.psl)

if ($verbose_flg) then
   echo $do_time $skl_dir/build/bin/sparkel_main $skli_flags $standard_library $debugger_srcs $*
endif
$do_time $skl_dir/build/bin/sparkel_main $skli_flags $standard_library $debugger_srcs $*
