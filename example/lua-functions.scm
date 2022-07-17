(begin
  (define start-time ((aref os "time")))
  ((aref os "execute") "sleep 3")
  (define end-time ((aref os "time")))
  (print "execution time" ((aref os "difftime") end-time start-time)))
