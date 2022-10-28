RUN="pryc aaa.pri"

echo "*** Simp_Loop"
$RUN simp_loop.pry -command Test_Simp_Loop 5

echo "*** Table"
$RUN table.pry -command Table 4

echo "*** Membership"
$RUN membership.pry -command Membership 2 3 5
$RUN membership.pry -command Membership 4 3 5

echo "*** Move"
$RUN move.pry -command Test_Move

echo "*** for_loop"
$RUN for_loop.pry -command Test_For_Loop 1 5

echo "*** func_result"
$RUN func_result.pry -command Func_Result 5

echo "*** annotations"
$RUN annotation.pry -command Test_Annotations

echo "*** simple_hello_world"
$RUN simple_hello_world.pry -command Hello_World

echo "*** case_stmt"
$RUN case_stmt.pry -command Test_Case 2
$RUN case_stmt.pry -command Test_Case 3
$RUN case_stmt.pry -command Test_Case 4
$RUN case_stmt.pry -command Test_Case 5

echo "*** string_case"
$RUN string_case.pry -command Test_String_Case "abc"
$RUN string_case.pry -command Test_String_Case "federick"
$RUN string_case.pry -command Test_String_Case "f"
$RUN string_case.pry -command Test_String_Case "g"
$RUN string_case.pry -command Test_String_Case "k"

echo "*** para_hello"
$RUN para_hello.pry -command Para_Hello2

echo "*** contin"
$RUN contin.pry -command Test_Continue

echo "*** prime_cycles"
$RUN prime_cycles.pry -command Power2_Cycles 10

echo "*** para_block"
$RUN para_block.pry -command Para_Block

echo "*** case_expr"
$RUN case_expr.pry -command Case_Expr 0 1 2
$RUN case_expr.pry -command Case_Expr 1 1 2
$RUN case_expr.pry -command Case_Expr 2 1 2

echo "*** simple_fib"
$RUN simple_fib.pry -command Test_Fib 5
$RUN simple_fib.pry -command Test_Fib 10

echo "*** multi_continue"
$RUN multi_continue.pry -command Multi_Continue 5 10

echo "*** parse_tree"
$RUN parse_tree.pry -command Test_Parse

echo "*** n_queens"
$RUN n_queens.pry -command Test_N_Queens 6
$RUN n_queens.pry -command Test_N_Queens 5
$RUN n_queens.pry -command Test_N_Queens 4

echo "*** hippo_game"
$RUN dgraph.pry hippo_game.pry -command Place_Hippos

echo "*** drinking_phils"
$RUN drinking_phils.pry -command Test_DP 5

