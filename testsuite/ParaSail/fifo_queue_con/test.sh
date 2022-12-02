# The locale specified by the environment affects sort order.
export Set LC_ALL=C
RUN="psli ../aaa.psi"

$RUN fifo_queue_con.psl -command Test_Fifo 20 10 30 |
  sort
