RUN="a2xi ../aaa.a2i"

echo | $RUN show_cond_val_nesting.a2x | grep "Error:"
