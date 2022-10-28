RUN="pryi ../aaa.pri"

$RUN string_case.pry -command Test_String_Case \"abc\"
$RUN string_case.pry -command Test_String_Case \"abcdef\"
$RUN string_case.pry -command Test_String_Case \"def\"
$RUN string_case.pry -command Test_String_Case \"xx\"
$RUN string_case.pry -command Test_String_Case \"yy\"
$RUN string_case.pry -command Test_String_Case \"zz\"

$RUN string_case.pry -command Test_String_Case \"f\"
$RUN string_case.pry -command Test_String_Case \"g\"
$RUN string_case.pry -command Test_String_Case \"h\"

$RUN string_case.pry -command Test_String_Case \"frank\"
