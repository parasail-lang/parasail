# The locale specified by the environment affects sort order
export Set LC_ALL=C

RUN="pryi ../aaa.pri"

$RUN -command Test_Clock |
  sort |
  grep -v random |
  grep -v "Elapsed time"
