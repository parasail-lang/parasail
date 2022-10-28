RUN="skli aaa.ski"

echo "*** Simp_Loop"
$RUN simp_loop.skl -command Test_Simp_Loop 5

echo "*** Table"
$RUN table.skl -command Table 4

echo "*** Membership"
$RUN membership.skl -command Membership 2 3 5
$RUN membership.skl -command Membership 4 3 5

echo "*** Move"
$RUN move.skl -command Test_Move

echo "*** for_loop"
$RUN for_loop.skl -command Test_For_Loop 1 5

echo "*** func_result"
$RUN func_result.skl -command Func_Result 5

echo "*** annotations"
$RUN annotation.skl -command Test_Annotations

echo "*** simple_hello_world"
$RUN simple_hello_world.skl -command Hello_World

echo "*** case_stmt"
$RUN case_stmt.skl -command Test_Case 2
$RUN case_stmt.skl -command Test_Case 3
$RUN case_stmt.skl -command Test_Case 4
$RUN case_stmt.skl -command Test_Case 5

echo "*** string_case"
$RUN string_case.skl -command Test_String_Case "abc"
$RUN string_case.skl -command Test_String_Case "federick"
$RUN string_case.skl -command Test_String_Case "f"
$RUN string_case.skl -command Test_String_Case "g"
$RUN string_case.skl -command Test_String_Case "k"

echo "*** para_hello"
$RUN para_hello.skl -command Para_Hello2

echo "*** contin"
$RUN contin.skl -command Test_Continue

echo "*** prime_cycles"
$RUN prime_cycles.skl -command Power2_Cycles 10

echo "*** para_block"
$RUN para_block.skl -command Para_Block

echo "*** case_expr"
$RUN case_expr.skl -command Case_Expr 0 1 2
$RUN case_expr.skl -command Case_Expr 1 1 2
$RUN case_expr.skl -command Case_Expr 2 1 2

echo "*** simple_fib"
$RUN simple_fib.skl -command Test 10

echo "*** multi_continue"
$RUN multi_continue.skl -command Multi_Continue 5 10

echo "*** parse_tree2"
$RUN parse_tree2.skl -command Test_Parse2

echo "*** n_queens"
$RUN n_queens.skl -command Test_N_Queens 8
$RUN n_queens.skl -command Test_N_Queens 6
$RUN n_queens.skl -command Test_N_Queens 4

echo "*** hippo_game"
$RUN dgraph.skl hippo_game.skl -command Place_Hippos

echo "*** drinking_phils"
$RUN drinking_phils.skl -command Test_DP 5

