# The locale specified by the environment affects sort order
export Set LC_ALL=C

RUN="skli ../aaa.ski"

$RUN -command Test_Clock |
  sort |
  grep -v "Elapsed time"
