RUN="a2xi ../aaa.a2i"

echo | $RUN invert_result.a2x | grep "Error:"
