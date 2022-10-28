Last update: 2013/09/18

This directory has the following scripts to compile/execute the tests.

         _do_test-pass.sh: Execute all the tests that pass
       _do_test-failed.sh: Compile all the tests that currently fail
  _do_test-unsupported.sh: Compile all the tests which are still unsupported

These scripts currently cover two purposes: compile/execute the tests and
classify all the tests in categories: PASS, FAIL, UNSUPPORTED.


The following sections summarize the problems found rewriting the ParaSail
examples in Sparkel.

***** Reference Manual

[d1] In the reference manual (section 2.6: Reserved Words) we have the
     reserved word "interface"??? It seems a bug since according to
     the Sparkel RM, Chapter 3 (Types and Objects), Sparkel does not
     need a reserved word for such purpose.

[d2] According to the grammar rules of the Sparkel RM (Section 3.1) the
     following type definition is not valid (because the rule
     record_type_definition defined in Section 3.1.1 does not
     allow "null"):

         type MySet is new Set with null record;

     However it is accepted by the compiler. Is this a bug in the RM
     or a bug in the compiler??? (I assume it is a bug in the RM).

***** Compiler

[c1] "exports" is NOT a reserved word of Sparked but it is required by
     the compiler in package bodies.

[c2] The compiler does not verify consistency at the end of a subprogram
     For example, the following compiles without errors:

       proc Hello_World () is
         Println("Hello world!");
       end func Hello_World;         -- Error

[c3] The example module_add_on.skl should compile and execute without
     errors because the actual object of the call is not abstract.
     However the compiler reports:

      module_add_on.skl:39:8: Error: Call on abstract operation requires
        polymorphic operand: XML_Image(Obj)
      1 codegen errors found

[c4] Enumeration types declaration: The compiler does not seem to
     support well enumeration type declarations. For example, the
     following two type declarations are rejected by the compiler
     and they should work fine according to the RM (Section 3.1.2,
     page 17)

        type Color1 is Enum_Types.Enum<[#red, #green, #blue]>;    -- FAIL
        type Color2 is (red, green, blue);                        -- FAIL

     Reproducible with test case_expr.skl

