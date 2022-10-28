RUN="a2xi ../aaa.a2i"

echo | $RUN qsort.a2x sort_regr_tests.a2x | grep "Error:"
