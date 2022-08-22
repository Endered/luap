(define (list-to-string lst)
  (if (null? lst) "" (.. (car lst) " " (list-to-string (cdr lst)))))

(let ((xs (list 1 2 3 4))
      (ys (list 1 2 4 8)))
  (print
   (list-to-string
    (map (lambda (x y) (+ x y)) xs ys))))
