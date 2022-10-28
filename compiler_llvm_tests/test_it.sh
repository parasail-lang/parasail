# edit this to point to your ParaSail top directory
ParaSail='../'

bin='build/bin/parasail_main'
aaa='examples/aaa/aaa*.ps?'
reflection='examples/reflection.ps?'
llvm='examples/llvm_printer.ps?'
compiler='examples/compiler.ps?'

pslc_local() {
   $ParaSail$bin $ParaSail$aaa $ParaSail$reflection $ParaSail$llvm $ParaSail$compiler "$@" -command Compile "$@"
}

pslc_local abs.psl a.psl cmp.psl fact.psl float_cmp.psl identity.psl less_than.psl mod.psl max.psl null.psl one.psl ops.psl fib.psl prime.psl real_int_mult.psl exp.psl main.psl
llc main.psl.ll
llc abs.psl.ll
llc a.psl.ll
llc cmp.psl.ll
llc fact.psl.ll
llc float_cmp.psl.ll
llc identity.psl.ll
llc less_than.psl.ll
llc mod.psl.ll
llc max.psl.ll
llc null.psl.ll
llc one.psl.ll
llc ops.psl.ll
llc fib.psl.ll
llc prime.psl.ll
llc real_int_mult.psl.ll
llc exp.psl.ll
clang -c -g *.s
cd ..; make compiled_main OBJS="compiler_llvm_tests/*.o" EXE="compiler_llvm_tests/a.out"
cd compiler_llvm_tests; ./a.out | diff correct.txt -
