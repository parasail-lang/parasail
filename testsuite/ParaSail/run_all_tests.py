import os 
import os.path
import sys
import pathlib
from os.path import exists

# defining resources paths
local_path = os.path.dirname(os.path.realpath(__file__))
install_path = os.path.join(local_path, '..', '..', 'install')
aaa = os.path.join(install_path, 'lib', 'aaa.psi')
psli = os.path.join(install_path, 'bin', 'psli')

# setting the ParaSail Language Interpreter (psli) on PATH
os.environ['PATH'] += ':'+psli

# making aaa.psi accessible to every test.sh instances
if not exists("aaa.psi"):
    os.symlink(aaa, "aaa.psi")

# caching failing tests report
failed_tests = []

# blacklisting hard to automate tests
blacklist = ["ParaSail",         # root dir
             "core_file",        # asks an input. not ideal to automate
             "core_io",          # asks an input. not ideal to automate
             "ttt_tyler_edited", # asks an input. not ideal to automate
             "tictactoe",        # asks an input. not ideal to automate
             "gtk_cairo",        # needs dependencie(s) not properly set, no main?
             "rubiks_gtk",       # needs dependencie(s) not properly set
             "nqueens_gtk",      # needs dependencie(s) not properly set
             "hello_gtk",        # needs dependencie(s) not properly set
             "hello_button"      # needs dependencie(s) not properly set
             ]

# looping on every test folder
for root, _, files in os.walk(local_path):
    folder = pathlib.PurePath(root)

    # skip blacklist item
    if folder.name in blacklist:
        continue

    # uncomment to make a single test
    #if folder.name != "imat_vec":
    #    continue

    # build command line
    cmd_str = os.path.join(root, "test.sh")

    # make test.sh script executable
    os.chmod(cmd_str, 0o0777)

    # changing the current directory to the test folder
    os.chdir(root)

    # execute the test
    ret = os.system(cmd_str)

    # if test fails, cache replication infos
    if(ret != 0):
        failed_tests.append(str(cmd_str) + " failed.\n" +
                            "Failing command is: " + cmd_str + "\n")

# if at least one test fail, we report and exit with an error.
# until all tests pass, github action should report failure     
if len(failed_tests) > 0:
    all_failed_msg = '\n'.join([elem for elem in failed_tests])
    sys.exit(all_failed_msg)