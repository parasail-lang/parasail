#
# Initial bash version: July, 2014
# Author: Javier Miranda
#
# Initial python version: February, 2023
# Translators: Tucker Taft, Olivier Henley
#
# Description:
#   This script is used to emulate the basic functionality of the
#   mailserver. It assumes that no test hangs forever!
#
# Requirement:
#   ParaSail must be available in the PATH
# ------------------------------------------------------------------------

import os
import os.path
import sys
from os.path import exists
from subprocess import run
from subprocess import getoutput
from subprocess import getstatusoutput
import subprocess
import shutil

from enum import Enum

# defining resources paths
local_path = os.path.dirname(os.path.realpath(__file__))
install_path = os.path.join(local_path, "..", "..", "install")

pwd = os.getcwd()
language = os.path.basename(os.path.normpath(pwd))
lib_dir = os.path.join(local_path, "..", "..", "lib")

class Runtests(Enum):
    Out = 0
    Passed = 1
    Errors = 2
    Log = 3

runtests = [
    os.path.join(pwd, "runtests.out"),
    os.path.join(pwd, "runtests.passed"),
    os.path.join(pwd, "runtests.errors"),
    os.path.join(pwd, "runtests.log")
]

class TestResult(Enum):
    Passed = 0
    Failed = 1
    Maybe = 2

class SkipTarget(Enum):
    DoNot = 0
    All = 1
    Compiler = 2
    UnsupportedTarget = 3

class Translator(Enum):
    Compiler = 1
    Interpreter= 2

def compile_rts_library():
    """
    Ensure that the runtime library is compiled
    """
    if not os.path.exists(lib_dir):
        print(f"error: directory {lib_dir} not found")
        sys.exit(-1)

    run(["tcsh", "../../bin/pslc.csh", "-b1" ,"-v"])

def set_runtime_library():
    """
    Check the availability of the link to the runtime library
    """
    def create_symbolic_link(path_to_lib):
        print(f"Creating a symbolic link to {path_to_lib}")
        run(["ln", "-s", f"{path_to_lib}"])

    if language == "ParaSail":
        if not os.path.exists("aaa.psi"):
            create_symbolic_link ("../../lib/aaa.psi")
    elif language == "Sparkel":
        if not os.path.exists("aaa.ski"):
            create_symbolic_link ("../../sparkel_examples/aaa.ski")
    elif language == "Ada202x":
        if not os.path.exists("aaa.a2i"):
            create_symbolic_link ("../../ada202x_examples/aaa.a2i")
    elif language == "Parython":
        if not os.path.exists("aaa.pri"):
            create_symbolic_link ("../../parython_examples/aaa.pri")
    elif language == "Javallel":
        if not os.path.exists("aaa.jli"):
            create_symbolic_link ("../../javallel_examples/aaa.jli")
    else:
        print(f"error: unsupported language ({language})")
        print("Warning: Most probably script was executed in wrong dir")
        print("  It must be executed in one of the following directories:")
        print("   - testsuite/ParaSail")
        print("   - testsuite/Sparkel")
        print("   - testsuite/Ada202x")
        print("   - testsuite/Parython")
        print("   - testsuite/Javallel")
        sys.exit(-1)

def init_runtests_files():
    """
    Create/reset all the runtests files
    """
    for rt in runtests:
        with open (rt, "w") as file:
            pass

def choose_test_script(dir, translator_choice):
    """
    Choose the test script and the reference output file based on the translator choice (Compiler, Interpreter)
    """
    test_script = "NONE"
    test_reference_out = "NONE"

    if translator_choice == Translator.Compiler:
        test_script = "test-c.sh"
        test_reference_out = "test-c.out"
    else:
        test_script = "test.sh"
        test_reference_out = "test.out"
    if not os.path.exists(test_reference_out):
        test_reference_out = "test.out"

    if not os.path.exists(test_script):
        log_to_file(runtests[Runtests.Errors.value], f"{dir} {test_script} {test_reference_out} ({test_script} not found)")

    return test_script, test_reference_out

def handle_test_opt(test_opt_filepath):
    """
    Handle the test.opt file and find out if we skip the test
    """
    skip_target = SkipTarget.DoNot
    if os.path.exists(test_opt_filepath):
        with open(test_opt_filepath, 'r') as input_file:
            for line in input_file:
                for w in line.split():
                    if w == "ALL" or w == "all":
                        skip_target = SkipTarget.All
                    elif w == "COMPILER" or w == "compiler":
                        if translator != Translator.Compiler:
                            skip_target = SkipTarget.Compiler
                    else:
                        skip_target = SkipTarget.UnsupportedTarget
                    break
                    
    return skip_target

def run_test(dir, test_script, test_reference_out):
    """
    Run a given test using its script file
    """
    if os.path.exists(test_script):
        log_to_file(runtests[Runtests.Log.value], f"{dir} {test_script} {test_reference_out} ")
        with open("tmp.out", 'w') as fp:
            run([os.path.join(os.getcwd(), test_script)], shell=True, stdout=fp, stderr=fp)

def print_test_status(num_tests, num_dirs_to_test, num_failed_tests, test_name, skip_target):
    """
    Inform of tests progress on cmd
    """
    skip_info = f" SKIPPED {skip_target.name}"
    if skip_target == SkipTarget.DoNot:
        skip_info = ""

    print(f"Num_Tests = {num_tests}/{num_dirs_to_test} (Failed = {num_failed_tests}) {test_name} {skip_info}")

def cleanup_tmp():
    shutil.rmtree(os.path.join(pwd, "tmp"), ignore_errors=True)

def create_tmp_dir_with_test_files(dir):
    """
    Recreate the temporary directory and fill it with the test files
    """
    cleanup_tmp()
    shutil.copytree(os.path.join(pwd, dir), os.path.join(pwd, "tmp"))

def move_to_tmp_dir():
    os.chdir("tmp")

def move_out_of_tmp_dir():
    os.chdir("..")

def log_to_file(filename, text_line):
    """
    Log a line of text to filename
    """
    log_f = open(filename, 'a', encoding='utf-8')
    log_f.write(text_line + '\n')

def check_test_output(test_reference_out, temp_out, out_file, dir):
    """
    Compare the test output with the reference output
    """
    if os.path.isfile(test_reference_out):
        cmd = f"diff -B -w -u {test_reference_out} {temp_out}"
        output = subprocess.run([cmd], shell=True, stdout=subprocess.PIPE)
        output_text = output.stdout.decode('utf-8')
        if output_text == '':
            log_to_file(runtests[Runtests.Passed.value], f" {dir}")
            return TestResult.Passed
        else:
            log_to_file(out_file, f" ************ TEST {dir} ************")
            log_to_file(out_file, output_text)
            return TestResult.Failed
        
    return TestResult.Maybe

def identify_translator():
    """
    Identify if we use the compiler or the interpreter
    """
    translator = Translator.Interpreter
    args = sys.argv[1:]
    if len(args) >= 1 and args[0] == "-c":
        translator = Translator.Compiler
        args = args[1:]

    if language == "ParaSail" and translator == Translator.Interpreter:
        os.environ["PATH"] += ":" + os.path.join(install_path, "bin")

    return translator, args

def identify_dirs_to_test(args_left_to_consume):
    """
    Characterize the dirs to test
    """

    dirs_to_test = args_left_to_consume if len(args_left_to_consume) > 0 else os.listdir('.')

    aux = []
    for dir in dirs_to_test:
        if os.path.isdir(dir):
            if dir != "support" and dir != "tmp.rts" and dir != "tmp":
                skip_target = handle_test_opt(os.path.join(pwd, dir, "test.opt"))
                aux.append((dir, skip_target))

    dirs_to_test = aux

    try:
        log_to_file(runtests[Runtests.Log.value], sys.argv[0])
        data = run(["uname", "-n"], capture_output=True, shell=True, text=True)
        log_to_file(runtests[Runtests.Log.value], data.stdout)
    except Exception as error:
        print("An exception occurred:", error)

    return dirs_to_test

def run_tests(translator, dirs_to_test):
    """
    Main loop where we run all the tests
    """
    print("Tests summary")
    print("-------------")

    num_tests_done = 0
    num_failed_tests = 0

    init_runtests_files()

    dirs_to_test.sort(key=lambda tup: tup[0]) 

    for test in dirs_to_test:
        test_dir = test[0]
        skip_target = test[1]

        if skip_target == SkipTarget.DoNot:
            if os.path.isdir(test_dir):


                create_tmp_dir_with_test_files(test_dir)

                move_to_tmp_dir()

                test_script, test_reference_out = choose_test_script(test_dir, translator)
                run_test(test_dir, test_script, test_reference_out)
                test_result = check_test_output(test_reference_out, "tmp.out", runtests[Runtests.Out.value], test_dir)
                if test_result == TestResult.Failed:
                    num_failed_tests = num_failed_tests + 1

                move_out_of_tmp_dir()
        else:
            log_to_file(runtests[Runtests.Log.value], f"{test_dir} SKIPPED {skip_target.name}")

        num_tests_done = num_tests_done + 1
        print_test_status(num_tests_done, len(dirs_to_test), num_failed_tests, test_dir, skip_target)

if __name__ == '__main__':

    if not os.path.isfile(os.path.join(lib_dir, "aaa.psi.o")):
        compile_rts_library()

    set_runtime_library()
    translator, args_left_to_consume = identify_translator()
    run_tests(translator, identify_dirs_to_test(args_left_to_consume))