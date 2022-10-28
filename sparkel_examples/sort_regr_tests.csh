#
set here = $cwd
cd ~/_parasail/sparkel_examples
sklc aaa.ski qsort.skl sort_regr_tests.skl -command Sort_Regr_Tests \"$here/$1\"
