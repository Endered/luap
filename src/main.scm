(use util.match)

(define *lua-transpile-macros* ())
(define *lua-transpile-objects-root* "LUA_TRANSPILE_OBJECTS_ROOT")

(define next-objects-root
  (let ((num 0))
    (lambda ()
      (let ((res (format #f "~a_~a" *lua-transpile-objects-root* num)))
	(set! num (+ num 1))
	res))))

(define (some x)
  (list x))

(define (none)
  (list))

(define (some? x)
  (and (pair? x)
       (null? (cdr x))))

(define (none? x)
  (null? x))

(define (get-some x)
  (car x))

(define (find-map-some f lst)
  (if (null? lst)
      (none)
      (let ((x (f (car lst))))
	(if (some? x)
	    x
	    (find-map-some f (cdr lst))))))

(define-syntax define-lua-syntax
  (syntax-rules ()
    ((_ (head . args) env then)
     (set! *lua-transpile-macros*
	   (cons
	    (lambda (expr env)
	      (match expr
		     (('head . args)
		      (some then))
		     (xs (none))))
	    *lua-transpile-macros*)))))

(define-lua-syntax (require path) env
  (format #f "(require ~a)" (transpile path env)))

(define-lua-syntax (let variables . body) env
  (transpile `((lambda ,(map car variables) ,@body) ,@(map cadr variables))
	     env))

(define-lua-syntax (begin . body) env
  (transpile `(let () ,@body)
	     env))

(define-lua-syntax (lambda args . body) env
  (let ((normal-lambda
	 (lambda ()
	   (format #f "function(~a)\n~a\nend"
		   (join-string ", " (map true-name args))
		   (transpile-same-scope
		    body
		    (append (map (lambda (x) (cons x x)) args) env)))))
	(variadib-lambda
	 (lambda ()
	   (let ((normal-args (reverse (reverse args)))
		 (variadic-arg (cdr (last-pair args))))
	     (format #f "function(~a,...)\nlocal ~a = ~a({...})\n~a\nend"
		     (join-string "," (map true-name normal-args))
		     (true-name variadic-arg)
		     (transpile 'array-to-list env)
		     (transpile-same-scope
		      body
		      (append (map (lambda (x) (cons x x)) normal-args)
			      (list (cons variadic-arg variadic-lambda))
			      env))))))
	(only-variadic-lambda
	 (lambda ()
	   (format #f "function(...)\nlocal ~a = ~a({...})\n~a\nend"
		   (true-name args)
		   (transpile 'array-to-list env)
		   (transpile-same-scope
		    body
		    (append (list (cons args args)) env))))))
    (cond ((null? args)
	   (normal-lambda))
	  ((not (list? args))
	   (only-variadic-lambda))
	  ((null? (cdr (last-pair args)))
	   (normal-lambda))
	  (else
	   (variadib-lambda)))))

(define-lua-syntax (define var expr) env
  (transpile `(set! ,var ,expr) env))

(define-lua-syntax (define (f . args) . body) env
  (transpile
   `(set! ,f (lambda ,args ,@body))
   env))

(define-lua-syntax (if condition then else) env
  (format #f "(function() if ~a then \nreturn ~a\nelse\nreturn ~a\nend\nend)()"
	  (transpile condition env)
	  (transpile then env)
	  (transpile else env)))

(define-lua-syntax (set! var expr) env
  (transpile
   `(begin
      (eval ,(format #f "~a = ~a" (transpile var env) (transpile expr env)))
      ,var)
   env))

(define-lua-syntax (aref var . indexes) env
  (format #f "~a~a"
	  (transpile var env)
	  (join-string "" (map (lambda (index)
				 (format #f "[~a]" index))
			       (map (lambda (index) (transpile index env)) indexes)))))

(define-lua-syntax (make-table . binds) env
  (format #f "{~a}"
	  (join-string
	   ",\n"
	   (map (lambda (c)
		  (format #f "~a = ~a" (true-name (car c)) (transpile (cadr c) env)))
		binds))))

(define-lua-syntax (make-array . exprs) env
  (format #f "{~a}"
	  (join-string
	   ","
	   (map (lambda (expr)
		  (transpile expr env))
		exprs))))

(define (join-string sep strings)
  (define (rec lists)
    (if (null? lists)
	""
	(format #f "~a~a~a" sep (car lists) (rec (cdr lists)))))
  (if (null? strings)
      ""
      (format #f "~a~a" (car strings) (rec (cdr strings)))))

(define-syntax define-binary-operator
  (syntax-rules ()
    ((_ symbol op)
     (define-lua-syntax (symbol . args) env
       (format #f "(~a)" (join-string
			  op
			  (map (lambda (expr)
				 (transpile expr env))
			       args)))))))

(define-binary-operator + " + ")
(define-binary-operator - " - ")
(define-binary-operator * " * ")
(define-binary-operator / " / ")
(define-binary-operator .. " .. ")
(define-binary-operator or " or ")
(define-binary-operator and " and ")

(define-lua-syntax (not x) env
  (format #f "(not ~a)" (transpile x env)))

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

(define-lua-syntax (= . args) env
  (let ((exprs (map (lambda (expr) (transpile expr env)) args)))
    (format #f "(~a)" (join-string " and "
				   (map (lambda (l r)
					  (format #f "~a == ~a" l r))
					exprs (cdr exprs))))))

(define-lua-syntax (/= . args) env
  (let ((exprs (map (lambda (expr) (transpile expr env)) args)))
    (format #f "(~a)" (join-string " and "
				   (map (lambda (l r)
					  (format #f "~a ~~= ~a" l r))
					exprs (cdr exprs))))))

(define-lua-syntax (lua-for (key value expr) . body) env
  (format #f "(function()\nfor ~a,~a in pairs(~a) do\n~a\nend\nend)()"
	  (true-name key)
	  (true-name value)
	  (transpile expr env)
	  (transpile `(begin ,@body) env)))

(define-lua-syntax (lua-ifor (key value expr) . body) env
  (format #f "(function()\nfor ~a,~a in ipairs(~a) do\n~a\nend\nend)()"
	  (true-name key)
	  (true-name value)
	  (transpile expr env)
	  (transpile `(begin ,@body) env)))

(define-lua-syntax (while condition . body) env
  (format #f "(function()\nwhile(~a)do\n~a\nend\nend)()"
	  (transpile condition env)
	  (transpile `(begin ,@body) env)))

(define-lua-syntax (eval code) env
  code)

(define (mappend f . args-list)
  (apply append (apply map f args-list)))

(define (var? expr)
  (symbol? expr))

(define (defined-var? expr env)
  (and (var? expr)
       (find-if (lambda (x)
		  (eq? (car x) expr))
		env)))

(define (to-lua-symbol x)
  (define (conv c)
    (cond ((eq? c #\-) "_HYPHEN_")
	  ((eq? c #\?) "_QUESTION_")
	  ((eq? c #\*) "_STAR_")
	  (else (list->string (list c)))))
  (list->string
   (mappend string->list
	    (map conv
		 (string->list
		  (format #f "~a" x))))))

(define (true-name var :optional (env ()))
  (to-lua-symbol
   (let ((x (find-if (lambda (x)
		       (eq? (car x) var))
		     env)))
     (if x (cdr x) var))))

(define (undefined-var? expr env)
  (and (var? expr)
       (not (find-if (lambda (x)
		       (eq? (car x) expr))
		     env))))

(define (transpile-nil expr env)
  (if (null? expr)
      (some "nil")
      (none)))

(define (transpile-defined-var expr env)
  (if (defined-var? expr env)
      (some (true-name expr env))
      (none)))

(define (transpile-undefined-var expr env)
  (if (undefined-var? expr env)
      (some (true-name expr))
      (none)))

(define (transpile-number expr env)
  (if (number? expr)
      (some (format #f "(~a)" expr))
      (none)))

(define (transpile-string expr env)
  (if (string? expr)
      (some (format #f "\"~a\"" expr))
      (none)))

(define (transpile-macros expr env)
  (find-map-some
   (lambda (f)
     (f expr env))
   *lua-transpile-macros*))

(define (transpile-function-call expr env)
  (some (format #f "(~a)(~a)"
		(transpile (car expr) env)
		(join-string "," (map (lambda (expr)
					(transpile expr env))
				      (cdr expr))))))

(define (transpile expr env)
  (get-some (find-map-some
	     (lambda (f)
	       (f expr env))
	     (list
	      transpile-nil
	      transpile-defined-var
	      transpile-undefined-var
	      transpile-number
	      transpile-string
	      transpile-macros
	      transpile-function-call))))

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
	    (let ((env (append (map (lambda (symbol)
				      (cons symbol
					    (format #f "~a.~a"
						    next-root
						    symbol)))
				    (find-all-define exprs))
			       env)))
	      (let ((evaled (map (lambda (expr)
				   (format #f "~a;" (transpile expr env)))
				 exprs)))
		(join-string
		 "\n"
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
   (make-table (car car) (cdr cdr) (type "cons")))
 (define (car cons)
   (aref cons "car"))
 (define (cdr cons)
   (aref cons "cdr"))
 (define (reverse list)
   (let ((res nil))
     (while (/= list nil)
	    (set! res (cons (car list) res))
	    (set! list (cdr list)))
     res))
 (define (array-to-list array)
   (let ((res nil))
     (lua-ifor (_ value array)
	       (set! res (cons value res)))
     (reverse res)))
 (define (null? x)
   (= x nil))
 (define (pair? x)
   (and (not (null? x))
	(= (aref x "type") "cons")))
 (define (list? x)
   (or (null? x) (pair? x)))
 (define (list . args)
   args)
 (define (map f list)
   (if (null? list)
       nil
       (cons (f (car list))
	     (map f (cdr list))))))

(display (transpile-same-scope (append global-programs (read-while-eof)) ()))

