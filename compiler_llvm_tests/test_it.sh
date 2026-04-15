ParaSail='../'
PSLC=$ParaSail'bin/pslc.csh'

# Clean up
rm -f *.o *.s *.ll a.out

# compile all tests to .o using INTERPRETER (-i) for stability
$PSLC -i -k abs.psl a.psl cmp.psl fact.psl float_cmp.psl identity.psl less_than.psl mod.psl max.psl null.psl one.psl ops.psl fib.psl prime.psl real_int_mult.psl exp.psl main.psl

# Link
(cd .. && make --silent compiled_main OBJS="compiler_llvm_tests/*.o" EXE="a.out")
mv ../a.out ./a.out

# Run
./a.out | diff correct.txt -
