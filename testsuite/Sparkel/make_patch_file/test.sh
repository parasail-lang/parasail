RUN="skli ../aaa.ski"

echo | $RUN make_patch_file.skl | grep "Error:"
