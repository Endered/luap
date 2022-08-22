(transpiler-eval
 (defmacro (for x '<- xs . body)
   `(mappend (lambda (,x) (for ,@body)) ,xs))
 (defmacro (for x ':= expr . body)
   `(let ((,x ,expr)) (for ,@body)))
 (defmacro (for 'if condition . body)
   `(if ,condition (for ,@body) ()))
 (defmacro (for 'do . body)
   `(begin ,@body ()))
 (defmacro (for 'yield . body)
   `(list (begin ,@body))))

(define (length lst)
  (if (null? lst)
      0
      (+ 1 (length (cdr lst)))))

(define (append . lsts)
  (define (rec1 lsts)
    (cond ((= 0 (length lsts)) ())
	  ((= 1 (length lsts)) (car lsts))
	  (else (rec2 (car lsts) (cdr lsts)))))
  (define (rec2 lst lsts)
    (if (null? lst)
	(rec1 lsts)
	(cons (car lst) (rec2 (cdr lst) lsts))))
  (rec1 lsts))

(define (mappend f lst)
  (apply append (map f lst)))

(define (range from to step)
  (if (> from to) ()
      (cons from (range (+ from step) to step))))

(define (nth n lst)
  (if (<= n 0)
      (car lst)
      (nth (- n 1) (cdr lst))))

(let ((triangles (for x <- (range 1 10 1)
		      y <- (range 1 10 1)
		      l := (+ (* x x) (* y y))
		      z <- (range 1 10 1)
		      r := (* z z)
		      if (= l r)
		      yield
		      (list x y z))))
  (for triangle <- triangles
       x := (nth 0 triangle)
       y := (nth 1 triangle)
       z := (nth 2 triangle)
       do
       (print x y z)))
