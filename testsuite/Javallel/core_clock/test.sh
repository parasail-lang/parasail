# The locale specified by the environment affects sort order
export Set LC_ALL=C

RUN="jlli ../aaa.jli"

$RUN -command testClock |
  sort |
  grep -v random |
  grep -v "Elapsed time"
