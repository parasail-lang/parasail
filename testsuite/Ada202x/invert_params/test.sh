RUN="a2xi ../aaa.a2i"

echo | $RUN invert_params.a2x | grep "Error:"
