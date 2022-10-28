(bin/test_distrib_vectors 100 5 3 ; echo "1st finished, status = $status") &
(bin/test_distrib_vectors 100 3 3 ; echo "2nd finished, status = $status") &
(bin/test_distrib_vectors 100 2 3 ; echo "3rd finished, status = $status") &
wait
