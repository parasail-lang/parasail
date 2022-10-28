RUN="skli ../aaa.ski"

echo | $RUN qsort.skl sort_regr_tests.skl | grep "Error:"
