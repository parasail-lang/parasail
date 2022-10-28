RUN="sklc ../aaa.ski"

echo "*** aggr (same errors in ParaSail example)"
$RUN aggr.skl

echo "*** module_add_on"
$RUN module_add_on.skl -command TestSet

echo "*** simple_apply"
$RUN simple_apply.skl

echo "*** div_con"
echo " This test fails because it does not perform the expected calls"
echo " at runtime. It fails also in ParaSail."
$RUN div_con.skl -command Div_Con 20 2

echo "*** box2"
$RUN box2.skl

echo "*** map_reduce_func_int_v1"
$RUN map_reduce_func_int_v1.skl 

echo "*** map_reduce_func_int_v2"
$RUN map_reduce_func_int_v2.skl 

echo "*** parse_tree"
$RUN parse_tree.skl 

echo "*** seach_binary_tree"
$RUN search_binary_tree.skl 
