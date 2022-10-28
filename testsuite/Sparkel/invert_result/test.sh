RUN="skli ../aaa.ski"

echo | $RUN invert_result.skl | grep "Error:"
