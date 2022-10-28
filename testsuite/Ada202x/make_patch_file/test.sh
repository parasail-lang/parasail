RUN="a2xi ../aaa.a2i"

echo | $RUN make_patch_file.a2x | grep "Error:"
