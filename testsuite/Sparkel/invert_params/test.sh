RUN="skli ../aaa.ski"

echo | $RUN invert_params.skl | grep "Error:"
