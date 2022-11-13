Quick summary:
--------------
Overall executing the testsuite is easy. You can run all the testsuite
tests with the compiler as follows:

  $ cd testsuite/ParaSail
  $ ../support/runtests.sh -c

The above command temporarily generates two subdirectories:

    testsuite/ParaSail/tmp
    testsuite/ParaSail/tmp.rts

The subdirectory tmp is used to compile and execute the test (similar to
the execution of the tests with the interpreters); the subdirectory tmp.rts
is used to locally rebuild the runtime files of ParaSail. Every time you
build the compiler or the interpreters the Makefile takes care of removing
this directory to force the runtests.sh script to rebuild the runtime of
the compiler (because your change to the sources of the compiler may
affect to the compilation of the runtime).

If you want to execute just one or several tests you can add the name
of their subdirectories to the above command line. For example:

  $ ../support/runtests.sh -c dgraph expr_tree

-------------------
Testsuite structure
-------------------

  The structure of this testsuite is compatible with the
structure used by the AdaCore mailservers (thus facilitating its future use by
a continuous integration tool). That is, each test is located in a directory
containing:

 a) all the sources needed to pass the test (except the runtime files of the
    language)
 b) the file "test.sh" (with the bash commands to execute the test)
 c) (optional) the file "test.out" (with the expected output, if any).
 d) (optional) the file "test.opt" (see section "Controlling test execution")

Building the language tools
---------------------------

  Before running the tests, the language tools must be built (interpreter,
compiler, etc.). Detailed instructions available in the root of the
repository in the file README.txt. For example, we can execute:

  $ cd <clone-of-the-repository>
  $ make

Running the tests
-----------------

  In order to locally run the tests, the following scripts are available in
the directory testsuite/support:

 - runalltests.sh  Bash script to run all the tests of the testsuite
 - runtests.sh     Bash script to run all the tests of a language or a
                   subset of tests
 - clean.sh        Bash script to remove temporary files created by the
                   previous scripts

  runalltests.sh invokes runtests.sh on subdirectories ParaSail, Sparkel,
Parython and Javallel to execute all their tests.

  runtests.sh relies on the PATH environment variable to locate the tools
needed to run the tests (ie. grep, sort, pslc, sklc, etc.). Given that most
we are generally interested in testing our locally built tools, the common
setting consist in prepending to this variable the location of our locally
built tools. For example:

  $ export PATH="$PWD/install/bin:$PATH"

  Now we are ready to run tests. For example, the following sequence of
command execute all the tests of ParaSail:

  $ cd testsuite/ParaSail
  $ ../support/runtests.sh

  We can also specify a subset of tests to be run by means of specifying
in the command line the names of the directories containing the selected
tests. For example, the following command executes only two tests:

  $ cd testsuite/ParaSail
  $ ../support/runtests.sh clock for_website

  For each test, runtests.sh does the following actions:
   1) Copy the full contents of the test in a local directory ./tmp
   2) Execute the test
   3) Compute the output differences

  All the output differences (if any) are available at the end its execution
in the file ./runtests.out

  The execution of runtests.sh can be aborted by pressing Ctrl-C

Updating the output of the tests
--------------------------------

In order to update the output of a test the recommended procedure
is to execute the test locally using runtest.sh, copy the new
output of the test into the directory containing its sources,
and update the repository.

For example, let us assume that we need to update the contents
of the ParaSail test named "apply_op". This is the full sequence:

1) Execute the test using runtest.sh

    $ cd testsuite/ParaSail
    $ ../support/runtests.sh apply_op

2) Analyze the output differences (generated in the file ./runtests.out
   only if the test execution generates output differences)

3) Copy the new output from ./tmp to the directory containing the test

    $ cp tmp/tmp.out apply_op/test.out

      Explanation: ./tmp is a directory used by the runtests.sh script
        to execute each test in a clean environment; all the sources of
        the test are copied to ./tmp before executing each test and the
        new output of the test is generated in ./tmp/tmp.out

4) Update the SVN repository

    $ svn ci


Controlling the execution of the test (test.opt) [minimum support yet]
------------------------------------------------

Test can be configured to be skipped, compiled, or run through the interpreter.
In order to handle fine control over configuration-dependent behavior, the test
writer can create a file called test.opt. The full syntax of test.opt is
described here:

  https://gaia.eu.adacore.com/doc/production/production_ug/gnatpython/test_opt.html

  Warning: Currently the script runtests.sh does not process the contents
    of test.opt. It just provides the following minimum functionality: if
    the file test.opt is found in the test, then the test is SKIPPED.

