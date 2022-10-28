@echo off

REM Interpret Ada202x files
REM Call with -h for help

set dir_called_from=%cd%

REM ~ means strip off any quotes
REM %~dp0 gives the full path of the directory containing the script (including
REM last backslash)
set bin_dir=%~dp0

cd %bin_dir%
REM Not sure of the point of this, unless it's to remove the last backslash
set bin_dir=%cd%

cd ..
set psl_dir=%cd%

cd %dir_called_from%

set version_string=Ada2x Interpreter version 9.3

goto next

:print_help
echo ada_interp.bat [flags] main.a2x [-command op param ...]
echo -t Time
echo -x eXplicit parallelism only
echo -p Parallelise automatically certain out-of-line calls
echo -v print Version and other Verbose output
echo -d print very verbose (Debug) output
echo -w print ParaSail virtual machine instructions
echo -h print this Help message
exit /b

:next

REM Give help if no parameters
if not "%1"=="" goto are_params
  echo %version_string%
  call :print_help
  goto :eof
:are_params

REM Default values

REM Ada202x interpreter flags
set psli_flags=

set verbose_flag=0

set do_time=

set parallelize_flag=0
set explicit_par_only_flag=0

:parse_flags
if "%~1"=="" goto end_parse_flags

if not "%~1"=="-x" goto not_x
  REM Tell Ada202x front end to not insert implicit parallelism
  set psli_flags=%psli_flags% -parcalls off
  set explicit_par_only_flag=1
  goto next_flag_please
:not_x

if not "%~1"=="-p" goto not_p
  REM Tell Ada202x front end to Parallelize
  set psli_flags=%psli_flags% -parcalls on
  set parallelize_flag=1
  goto next_flag_please
:not_p

if not "%~1"=="-w" goto not_w
  REM Tell Ada202x front end to dump Parasail VM instructions
  set psli_flags=%psli_flags% -listing on
  goto next_flag_please
:not_w

if not "%~1"=="-t" goto not_t
  REM Record Time
  set do_time=time
  goto next_flag_please
:not_t

if not "%~1"=="-v" goto not_v
  REM Print Version info and set Verbose
  echo %version_string%
  set verbose_flag=1
  goto next_flag_please
:not_v

if not "%~1"=="-d" goto not_d
  REM Debug
  echo %version_string%
  set psli_flags=%psli_flags% -debug on
  set verbose_flag=1
  goto next_flag_please
:not_d

if not "%~1"=="-h" goto not_h
  REM Help
  echo %version_string%
  call :print_help
  goto :eof
:not_h

REM echo ~x gives the file eXtension
if not "%~x1%"==".a2x" goto not_source
  set source_file=%~1
  goto end_parse_flags
:not_source

:next_flag_please

shift
goto parse_flags
:end_parse_flags

REM Extract optional command.  Can't use %* as it would give all arguments,
REM including those already processed.
set optional_command=
set optional_op=
set optional_param_1=
set optional_param_2=
if "%~2%"=="" goto no_more_command
  if "%~2%"=="-command" goto is_command
    echo -command expected
    echo %version_string%
    call :print_help
    goto :eof
  :is_command
  set optional_command=%~2%
  if "%~3%"=="" goto no_more_command
    set optional_op=%~3%
    if "%~4%"=="" goto no_more_command
      set optional_param_1=%~4%
      if "%~5%"=="" goto no_more_command
        set optional_param_2=%~%
:no_more_command

if not %parallelize_flag%==0 goto auto
if not %explicit_par_only_flag%==0 goto auto
REM Turn off automatic parallelization unless explicitly requested
set psli_flags=%psli_flags% -parcalls off
:auto

set standard_library=%psl_dir%\lib\aaa.a2i

set debugger_srcs=-lang parasail %psl_dir%\lib\aaa.psi
set debugger_srcs=%debugger_srcs% %psl_dir%\lib\reflection.ps?
set debugger_srcs=%debugger_srcs% %psl_dir%\lib\psvm_debugging.ps?
set debugger_srcs=%debugger_srcs% %psl_dir%\lib\debugger_console.psl

if %verbose_Flag%==0 goto not_verbose
  echo %psl_dir%\build\bin\ada202x_main %psli_flags% %standard_library% %debugger_srcs% -lang ada202x %source_file% %optional_command% %optional_op% %optional_param_1% %optional_param_2%
:not_verbose

if not "%do_time%"=="time" goto no_time_1
  echo %time%
:no_time_1
%psl_dir%\build\bin\ada202x_main %psli_flags% %standard_library% %debugger_srcs% -lang ada202x %source_file% %optional_command% %optional_op% %optional_param_1% %optional_param_2%
if not "%do_time%"=="time" goto no_time_2
  echo %time%
:no_time_2

