(define (max x y)
  (if (< x y)
      y
      x))
(define (min x y)
  (if (< x y)
      x
      y))

(define (clamp x low high)
  (max (min x high) low))

(define (floor x)
  ((aref math "floor") x))

(define (upto a b)
  (if (> a b)
      nil
      (cons a (upto (+ a 1) b))))

(define (under-n n)
  (upto 1 n))


(define (range from to step)
  (if (> from to)
      nil
      (cons from (range (+ from step) to step))))

(define (range-map f from to step)
  (map f (range from to step)))

(define (complex real imag)
  (cons real imag))

(define (complex-real c)
  (car c))
(define (complex-imag c)
  (cdr c))

(define (complex-add c1 c2)
  (complex
   (+ (complex-real c1) (complex-real c2))
   (+ (complex-imag c1) (complex-imag c2))))

(define (complex-sub c1 c2)
  (complex
   (- (complex-real c1) (complex-real c2))
   (- (complex-imag c1) (complex-imag c2))))

(define (complex-mul c1 c2)
  (let ((r1 (complex-real c1))
	(r2 (complex-real c2))
	(i1 (complex-imag c1))
	(i2 (complex-imag c2)))
    (complex
     (- (* r1 r2) (* i1 i2))
     (+ (* r1 i2) (* i1 r2)))))

(define (complex-inv c)
  (let ((r (complex-real c))
	(i (complex-imag c)))
    (let ((s (+ (* r r) (* i i))))
      (complex
       (/ r s)
       (/ (- 0 i) s)))))

(define (complex-div c1 c2)
  (complex-mul c1 (complex-inv c2)))

(define (complex-abs c)
  (let ((r (complex-real c))
	(i (complex-imag c)))
    (+ (* r r) (* i i))))

(define (nth n lst)
  (if (<= n 0)
      (car lst)
      (nth (- n 1) (cdr lst))))

(define (color r g b)
  (list r g b))

(define (color-to-string c)
  (.. (tostring (nth 0 c))
     " "
     (tostring (nth 1 c))
     " "
     (tostring (nth 2 c))))

(define mandel-max-depth 100)

(define mandel-max-dist 1e8)

(define (mandel-color c)
  (define (get-color n)
    (let ((v (floor (clamp (* (/ n 1.0 mandel-max-depth) 255) 0 255))))
      (color v v v)))
  (define (rec p depth)
    (if (or (>= depth mandel-max-depth)
	    (< mandel-max-dist (complex-abs p)))
	(get-color depth)
	(rec (complex-add (complex-mul p p) c)
	     (+ depth 1))))
  (rec (complex 0.0 0.0) 0))

(define (length lst)
  (if (null? lst)
      0
      (+ 1 (length (cdr lst)))))

(define xs (range -1.5 1.5 0.005))
(define ys (range -1.5 1.5 0.005))

(print "P3")
(print (length xs) (length ys))
(print 255)


(map
 (lambda (y)
   (map
    (lambda (x)
      (print
       (color-to-string
	(mandel-color (complex x y)))))
    xs))
 ys)
