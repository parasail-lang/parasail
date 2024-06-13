# Makefile used at AdaCore to build ParaSail compiler/interpreter
# local installation ready to use
# You must run this Makefile from the top level source directory

# The OMP library is enabled from the command line as follows:
#   $ make OMP=on

# The behavior of gdb can be improved compiling without optimizations. This
# is the default; optimization can be turned on with from the command line
# using:
#   $ make DEBUG=off

ifeq ($(DEBUG),off)
  ADAFLAGS=-p -g -O2 -gnatn -gnata -gnatE
else
  ADAFLAGS=-p -g -O0 -gnato -gnata -gnatE
endif
#  -gnat95 no longer supported

# Turn off implicit rules
.SUFFIXES:

PROCS=0
RM=rm -rf

# default to not silent, can be set to true from command line
# used by pslc.csh so that users aren't confused by walls of text
SILENT=0
ifeq ($(SILENT),1)
   GPRBUILD=gprbuild -q -p
else
   GPRBUILD=gprbuild -p
endif

OBJS=
EXE=compiled_main.exe
A2X_EXE=compiled_a2x_main.exe
OMPLIB=-lgomp
MATHLIB=-lm

ifeq ($(OMP),on)
  EXTRALIBS=$(OMPLIB)
else
  EXTRALIBS=
endif

# EXTRALIBS can be extended with additional libraries (if required)

COMPLIBS=$(MATHLIB)
A2X_COMPLIBS=$(MATHLIB)

.DELETE_ON_ERROR:

.PHONY: build_with_gtk all_with_gtk all local-install clean parasail parser 
.PHONY: sparkel sparkel_parser build_sparkel sparkel_doc
.PHONY: ada202x ada202x_parser build_ada202x ada202x_doc
.PHONY: parasail_gtk build
.PHONY: javallel javallel_parser build_javallel
.PHONY: parython parython_gtk parython_parser build_parython build_parython_gtk
.PHONY: test_runtime
.PHONY: compiled_main compiled_main_with_interp
.PHONY: do_psltags do_atags

all: config build do_psltags do_atags build_ada202x doc build_sparkel build_javallel build_parython ada202x_doc sparkel_doc test_runtime local-install

all_with_gtk: config build do_psltags do_atags build_ada202x doc build_sparkel build_javallel build_parython_gtk parasail_gtk ada202x_doc sparkel_doc test_runtime local-install

config: interpreter/psc-interpreter-locks.ads semantics/psc-link_names.ads

interpreter/psc-interpreter-locks.ads : interpreter/config.sh
	cd interpreter; ./config.sh $(OMP)

semantics/psc-link_names.ads : semantics/psc-link_names-common.ads
	cp semantics/psc-link_names-common.ads semantics/psc-link_names.ads

build_with_gtk: parser parasail check_compiled_main parasail_gtk

build: parser check_compiled_main parasail

build_sparkel: sparkel_parser sparkel

build_ada202x: ada202x_parser ada202x check_compiled_a2x_main

build_javallel: javallel_parser javallel

build_parython: parython_parser parython

build_parython_gtk: build_parython parython_gtk

do_atags : tags

tags : */*.ad?
	tcsh share/tools/vi_tags/do_atags.csh

do_psltags : lib/tags

lib/tags : lib/aaa.psi lib/reflection.ps? lib/llvm_printer.ps? lib/compiler.ps? lib/psvm_debugging.psl lib/parascope.ps? lib/vn_il.ps? lib/type_desc_llvm_utils.ps?
	cd lib; ../share/tools/vi_tags/psltags aaa.psi reflection.ps? llvm_printer.ps? compiler.ps? psvm_debugging.psl parascope.ps? vn_il.ps? type_desc_llvm_utils.ps?

doc:
	$(MAKE) -C documentation/ref_manual parasail_ref_manual.pdf

sparkel_doc:
	$(MAKE) -C documentation/sparkel_ref_manual sparkel_ref_manual.pdf

ada202x_doc:
	$(MAKE) -C documentation/ada202x_ref_manual ada202x_ref_manual.pdf

parser:
	$(MAKE) -C parser/build

sparkel_parser:
	$(MAKE) -C sparkel_parser/build

ada202x_parser:
	$(MAKE) -C ada202x_parser/build

javallel_parser:
	$(MAKE) -C javallel_parser/build

parython_parser:
	$(MAKE) -C parython_parser/build

parasail: cleantestsuite
	@mkdir -p build/bin build/obj build/slow_obj build/fast_obj
	$(GPRBUILD) $(ADAFLAGS) -P build/parasail -largs $(EXTRALIBS)

parasail_gtk: gtk_libs
	@mkdir -p build/bin build/obj build/slow_obj build/fast_obj
	-$(GPRBUILD) $(ADAFLAGS) -P build/parasail_gtk -largs $(EXTRALIBS)

sparkel: cleantestsuite
	@mkdir -p build/bin build/obj build/slow_obj build/fast_obj
	$(GPRBUILD) $(ADAFLAGS) -P build/sparkel -largs $(EXTRALIBS)

ada202x: cleantestsuite
	@mkdir -p build/bin build/obj build/slow_obj build/fast_obj
	$(GPRBUILD) $(ADAFLAGS) -P build/ada202x -largs $(EXTRALIBS)

javallel: cleantestsuite
	@mkdir -p build/bin build/obj build/slow_obj build/fast_obj
	$(GPRBUILD) $(ADAFLAGS) -P build/javallel -largs $(EXTRALIBS)

parython: cleantestsuite
	@mkdir -p build/bin build/obj build/slow_obj build/fast_obj
	$(GPRBUILD) $(ADAFLAGS) -P build/parython -largs $(EXTRALIBS)

parython_gtk: gtk_libs
	@mkdir -p build/bin build/obj build/slow_obj build/fast_obj
	-$(GPRBUILD) $(ADAFLAGS) -P build/parython_gtk -largs $(EXTRALIBS)

test_runtime:
	@mkdir -p build/bin build/obj build/slow_obj build/fast_obj
	$(GPRBUILD) $(ADAFLAGS) -P build/test_runtime -largs $(EXTRALIBS)

gtk_libs:
	mkdir -p build/obj build/slow_obj build/fast_obj
	-pkg-config gtk+-2.0 gthread-2.0 --libs > gtk_libs
	-rm -f build/obj/gtk_libs
	-cp gtk_libs build/obj/gtk_libs

check_compiled_main:
	$(GPRBUILD) -c $(ADAFLAGS) -P build/compiled_main -largs $(COMPLIBS)
	$(GPRBUILD) -c $(ADAFLAGS) -P build/compiled_main_with_interp -largs $(COMPLIBS)

compiled_main:
	rm -f $(EXE)
	$(GPRBUILD) $(ADAFLAGS) -P build/compiled_main -o $(EXE) -largs $(OBJS) $(COMPLIBS)

compiled_main_with_interp:
	rm -f $(EXE)
	$(GPRBUILD) $(ADAFLAGS) -P build/compiled_main_with_interp -o $(EXE) -largs $(OBJS) $(COMPLIBS)

check_compiled_a2x_main:
	$(GPRBUILD) -c $(ADAFLAGS) -P build/compiled_a2x_main -largs $(A2X_COMPLIBS)
	$(GPRBUILD) -c $(ADAFLAGS) -P build/compiled_a2x_main_with_interp -largs $(A2X_COMPLIBS)

compiled_a2x_main:
	rm -f $(A2X_EXE)
	$(GPRBUILD) $(ADAFLAGS) -P build/compiled_a2x_main -o $(A2X_EXE) -largs $(OBJS) $(A2X_COMPLIBS)

compiled_a2x_main_with_interp:
	rm -f $(A2X_EXE)
	$(GPRBUILD) $(ADAFLAGS) -P build/compiled_a2x_main_with_interp -o $(A2X_EXE) -largs $(OBJS) $(A2X_COMPLIBS)

local-install:
	$(RM) install/share/examples/parasail
	$(RM) install/share/doc/parasail
	$(RM) install/share/tools
	$(RM) install/bin
	$(RM) install/lib
	@mkdir -p install/bin
	@mkdir -p install/lib
	@mkdir -p install/lib/aaa
	@mkdir -p install/share
	@mkdir -p install/share/examples/parasail
	@mkdir -p install/share/doc/parasail
	@mkdir -p install/share/tools/parasail
	-cp -p documentation/ref_manual/*.pdf install/share/doc/parasail
	-cp -p documentation/parasail_release_notes*.txt install/share/doc/parasail
	-cp -p documentation/*.pdf install/share/doc/parasail
	-cp -p examples/*.ps? install/share/examples/parasail
	-cp -p lib/*.ps? install/lib
	-cp -p lib/aaa/*.ps?  install/lib/aaa
	-cp -p bin/pslc.csh install/bin/pslc.csh
	-cp -p bin/interp.csh install/bin/interp.csh
	-cp -p bin/scope.csh install/bin/scope.csh
	-cp -p build/bin/parasail_main_gtk install/bin/psli_with_gtk
	-cp -p build/bin/parasail_main install/bin/psli
	-cp -pr share/tools/* install/share/tools/parasail
	$(RM) install/share/examples/sparkel
	$(RM) install/share/doc/sparkel
	@mkdir -p install/share/examples/sparkel
	@mkdir -p install/share/doc/sparkel
	@mkdir -p install/share/tools/sparkel
	-cp -p documentation/sparkel_ref_manual/*.pdf install/share/doc/sparkel
	-cp -p documentation/sparkel_release_notes*.txt install/share/doc/sparkel
	-cp -p sparkel_examples/*.sk? install/share/examples/sparkel
	-cp -p build/bin/sparkel_main install/bin/skli
	-cp -p share/sparkel_tools/* install/share/tools/sparkel
	$(RM) install/share/examples/ada202x
	$(RM) install/share/doc/ada202x
	@mkdir -p install/share/examples/ada202x
	@mkdir -p install/share/tools/ada202x
	@mkdir -p install/share/doc/ada202x
	-cp -p documentation/ada202x_ref_manual/*.pdf install/share/doc/ada202x
	-cp -p documentation/ada202x_release_notes*.txt install/share/doc/ada202x
	-cp -p ada202x_examples/*.a2? install/share/examples/ada202x
	-cp -p build/bin/ada202x_main install/bin/a2xi
	-cp -p share/ada202x_tools/* install/share/tools/ada202x
	$(RM) install/share/examples/javallel
	$(RM) install/share/doc/javallel
	@mkdir -p install/share/examples/javallel
	@mkdir -p install/share/doc/javallel
	@mkdir -p install/share/tools/javallel
	-cp -p javallel_examples/*.jl? install/share/examples/javallel
	-cp -p build/bin/javallel_main install/bin/jlli
	$(RM) install/share/examples/parython
	$(RM) install/share/doc/parython
	@mkdir -p install/share/examples/parython
	@mkdir -p install/share/doc/parython
	@mkdir -p install/share/tools/parython
	-cp -p parython_examples/*.pr? install/share/examples/parython
	-cp -p build/bin/parython_main install/bin/pryi
	-cp -p build/bin/parython_main_gtk install/bin/pryi_gtk
	-cp -p build/bin/test_runtime install/bin/test_runtime

cleantestsuite:
	cd testsuite/ParaSail; ../support/clean.sh
	cd testsuite/Sparkel;  ../support/clean.sh
	cd testsuite/Parython; ../support/clean.sh
	cd testsuite/Javallel; ../support/clean.sh

cleanconfig:
	-rm interpreter/psc-interpreter-locks.ads
	-rm interpreter/psc-interpreter-locks.adb

clean: cleanconfig
	-gnatclean -q -r -P build/parasail
	$(RM) install gtk_libs
	$(RM) build/bin build/obj build/slow_obj build/fast_obj
	$(MAKE) -C documentation/ref_manual clean
	$(MAKE) -C parser/build clean-all
	-gnatclean -q -r -P build/sparkel
	-gnatclean -q -r -P build/ada202x
	-gnatclean -q -r -P build/javallel
	-gnatclean -q -r -P build/parython
	-gnatclean -q -r -P build/test_runtime
	$(MAKE) -C documentation/sparkel_ref_manual clean
	$(MAKE) -C sparkel_parser/build clean
	$(MAKE) -C ada202x_parser/build clean
	$(MAKE) -C javallel_parser/build clean
	$(MAKE) -C parython_parser/build clean
