RUN="skli ../aaa.ski"

echo | $RUN show_cond_val_nesting.skl | grep "Error:"
