#
set here = "$cwd"
cd ~/_parasail/sparkel_examples
foreach i ($*)
  echo sklc aaa.ski qsort.skl sort_regr_tests.skl \
                                     -command Sort_Regr_Tests \"$here/$i\"
  sklc aaa.ski qsort.skl sort_regr_tests.skl \
                                     -command Sort_Regr_Tests \"$here/$i\"
end
