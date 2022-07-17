(define (upto a b)
  (if (> a b)
      nil
      (cons a (upto (+ a 1) b))))

(define (under-n n)
  (upto 1 n))

(define (square n)
  (* n n))

(let ((xs (under-n 10)))
  (let ((ys (map square xs)))
    (map print ys)))
