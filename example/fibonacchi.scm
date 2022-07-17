(define (fibonacchi n)
  (if (<= n 0) 0
      (if (<= n 1) 1
	  (+ (fibonacchi (- n 2))
	     (fibonacchi (- n 1))))))


(print (fibonacchi 10))
