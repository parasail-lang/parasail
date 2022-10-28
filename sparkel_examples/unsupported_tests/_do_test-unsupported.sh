RUN="sklc ../aaa.ski"

echo "*** Multi_Output (multiple outputs)"
$RUN multi_outputs.skl

echo "*** Multi_Assign (aggregate as LHS of assignment)"
$RUN multi_assign.skl

echo "*** Poly_Type (multiple polymorphic controlling operands)"
$RUN poly_type.skl -command Poly_Type

echo "*** Type_Conv (type conversion not supported)"
$RUN type_conv.skl

echo "*** Box (default for formals not supported)"
$RUN box.skl
