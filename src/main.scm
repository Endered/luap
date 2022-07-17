(use util.match)

(define *lua-transpile-macros* ())
(define *lua-transpile-objects-root* "LUA_TRANSPILE_OBJECTS_ROOT")
(define *lua-temporal-object* "LUA_TEMPORAL_OBJECT")

(define next-objects-root
  (let ((num 0))
    (lambda ()
      (let ((res (format #f "~a_~a" *lua-transpile-objects-root* num)))
	(set! num (+ num 1))
	res))))

(define-syntax define-lua-syntax
  (syntax-rules ()
    ((_ (head . args) then)
     (set! *lua-transpile-macros*
	   (cons
	    (cons (lambda (expr env)
		    (match expr (('head . args) #t)
			   (xs #f)))
		  (lambda (expr env)
		    (match expr (('head . args) (list then))
			   (xs #f))))
	    *lua-transpile-macros*)))
    ((_ (head . args) env then)
     (set! *lua-transpile-macros*
	   (cons
	    (cons (lambda (expr env)
		    (match expr (('head . args) #t)
			   (xs #f)))
		  (lambda (expr env)
		    (match expr (('head . args) (list then))
			   (xs #f))))
	    *lua-transpile-macros*)))))

(define-lua-syntax (let variables . body) env
  (transpile `((lambda ,(map car variables) ,@body) ,@(map cadr variables))
	     env))

(define-lua-syntax (begin . body) env
  (transpile `(let () ,@body)
	     env))

(define-lua-syntax (lambda args . body) env
  (format #f "function(~a)\n~a\nend"
	  (join-string ", " args)
	  (transpile-same-scope body (append (map (lambda (x) (cons x "")) args) env))))

(define-lua-syntax (define var expr) env
  (format #f "~a = ~a" (transpile var env) (transpile var env)))

(define-lua-syntax (if condition then else) env
  (format #f "(function() if ~a then \nreturn ~a\nelse\nreturn ~a\nend\nend)()"
	  (transpile condition env)
	  (transpile then env)
	  (transpile else env)))

(define-lua-syntax (set! var expr) env
  (format #f "~a = ~a" (transpile var env) (transpile expr env)))

(define-lua-syntax (aref var . indexes) env
  (format #f "~a~a"
	  (transpile var env)
	  (join-string "" (map (lambda (index)
				 (format #f "[~a]" index))
			       (map (lambda (index) (transpile index env)) indexes)))))

(define-lua-syntax (create-table . binds) env
  (format #f "{~a}"
	  (join-string
	   ","
	   (map (lambda (c)
		  (format #f "~a = ~a" (car c) (transpile (cadr c) env)))
		binds))))

(define (join-string sep strings)
  (define (rec lists)
    (if (null? lists)
	""
	(format #f "~a~a~a" sep (car lists) (rec (cdr lists)))))
  (if (null? strings)
      ""
      (format #f "~a~a" (car strings) (rec (cdr strings)))))

(define-lua-syntax (define (f . args) . body) env
  (format #f "~a = ~a"
	  (transpile f env)
	  (transpile `(lambda ,args ,@body) env)))

(define-lua-syntax (+ . args) env
  (format #f "(~a)" (join-string " + " (map
					(lambda (expr)
					  (transpile expr env))
					args))))
(define-lua-syntax (- . args) env
  (format #f "(~a)" (join-string " - " (map
					(lambda (expr)
					  (transpile expr env))
					args))))
(define-lua-syntax (* . args) env
  (format #f "(~a)" (join-string " * " (map
					(lambda (expr)
					  (transpile expr env))
					args))))
(define-lua-syntax (/ . args) env
  (format #f "(~a)" (join-string " / " (map
					(lambda (expr)
					  (transpile expr env))
					args))))

(define-lua-syntax (or . args) env
  (format #f "(~a)" (join-string " or " (map
					(lambda (expr)
					  (transpile expr env))
					args))))

(define-lua-syntax (and . args) env
  (format #f "(~a)" (join-string " and " (map
					(lambda (expr)
					  (transpile expr env))
					args))))

(define-lua-syntax (< . args) env
  (let ((exprs (map (lambda (expr) (transpile expr env)) args)))
    (format #f "(~a)" (join-string " and "
				   (map (lambda (l r)
					  (format #f "~a < ~a" l r))
					exprs (cdr exprs))))))

(define-lua-syntax (<= . args) env
  (let ((exprs (map (lambda (expr) (transpile expr env)) args)))
    (format #f "(~a)" (join-string " and "
				   (map (lambda (l r)
					  (format #f "~a <= ~a" l r))
					exprs (cdr exprs))))))

(define-lua-syntax (> . args) env
  (let ((exprs (map (lambda (expr) (transpile expr env)) args)))
    (format #f "(~a)" (join-string " and "
				   (map (lambda (l r)
					  (format #f "~a > ~a" l r))
					exprs (cdr exprs))))))

(define-lua-syntax (>= . args) env
  (let ((exprs (map (lambda (expr) (transpile expr env)) args)))
    (format #f "(~a)" (join-string " and "
				   (map (lambda (l r)
					  (format #f "~a >= ~a" l r))
					exprs (cdr exprs))))))

(define (mappend f . args-list)
  (apply append (apply map f args-list)))

(define (var? expr)
  (symbol? expr))

(define (defined-var? expr env)
  (and (var? expr)
       (find-if (lambda (x)
		  (eq? (car x) expr))
		env)))

(define (true-name var env)
  (format #f "~a~a"
	  (cdr (find-if (lambda (x)
			  (eq? (car x) var))
			env))
	  var))

(define (undefined-var? expr env)
  (and (var? expr)
       (not (find-if (lambda (x)
		       (eq? (car x) expr))
		     env))))

(define (transpile expr env)
  (cond ((defined-var? expr env)
	 (true-name expr env))
	((undefined-var? expr env)
	 (format #f "~a" expr))
	((number? expr)
	 (format #f "(~a)" expr))
	((string? expr)
	 (format #f "\"~a\"" expr))
	((find-if (lambda (f)
		    ((car f) expr env))
		  *lua-transpile-macros*)
	 (car ((cdr (find-if (lambda (f)
			       ((car f) expr env))
			     *lua-transpile-macros*))
	       expr
	       env)))
	(else
	 (format #f "(~a)(~a)\n"
		 (transpile (car expr) env)
		 (join-string "," (map (lambda (expr)
					 (transpile expr env))
				       (cdr expr)))))))

(define (find-if f list)
  (if (null? list)
      #f
      (let ((v (f (car list))))
	(if v (car list) (find-if f (cdr list))))))

(define (remove-last list)
  (reverse (cdr (reverse list))))

(define (transpile-same-scope exprs env)
  (define (find-all-define exprs)
    (mappend (match-lambda (('define (var . _) . _) (list var))
			   (('define var _) (list var))
			   (_ ()))
	     exprs))
  (let ((next-root (next-objects-root)))
    (format #f "local ~a = {}\n~a\n"
	    next-root
	    (let ((env (append (map (lambda (symbol) (cons symbol (format #f "~a." next-root)))
				    (find-all-define exprs)) env)))
	      (let ((evaled (map (lambda (expr) (transpile expr env))
				 exprs)))
		(join-string
		 "\n_=nil\n"
		 (append (remove-last evaled)
			 (list (format #f "return ~a" (last evaled))))))))))

(define (read-while-eof)
  (let ((res (read)))
    (if (eof-object? res)
	()
	(cons res (read-while-eof)))))

(define global-programs ())

(define-syntax register-program
  (syntax-rules ()
    ((_ . body)
     (set! global-programs (append global-programs 'body)))))


(register-program
 (define (cons car cdr)
   (create-table (car car) (cdr cdr)))
 (define (car cons)
   (aref cons "car"))
 (define (cdr cons)
   (aref cons "cdr")))

(display (transpile-same-scope (append global-programs (read-while-eof)) ()))

