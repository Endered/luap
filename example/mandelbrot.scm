(use util.match)

(define *lua-transpile-macros* ())
(define *lua-transpile-objects-root* "LUA_TRANSPILE_OBJECTS_ROOT")

(define next-objects-root
  (let ((num 0))
    (lambda ()
      (let ((res (format #f "~a_~a" *lua-transpile-objects-root* num)))
	(set! num (+ num 1))
	res))))

(define-syntax define-lua-syntax
  (syntax-rules ()
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
		    (append (map (lambda (x) (cons x "")) args) env)))))
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
		      (append (map (lambda (x) (cons x "")) normal-args)
			      (list (cons variadic-arg ""))
			      env))))))
	(only-variadic-lambda
	 (lambda ()
	   (format #f "function(...)\nlocal ~a = ~a({...})\n~a\nend"
		   (true-name args)
		   (transpile 'array-to-list env)
		   (transpile-same-scope
		    body
		    (append (list (cons args "")) env))))))
    (cond ((null? args)
	   (normal-lambda))
	  ((not (list? args))
	   (only-variadic-lambda))
	  ((null? (cdr (last-pair args)))
	   (normal-lambda))
	  (else
	   (variadib-lambda)))))

(define-lua-syntax (define var expr) env
  (format #f "~a = ~a" (transpile var env) (transpile expr env)))

(define-lua-syntax (define (f . args) . body) env
  (format #f "~a = ~a"
	  (transpile f env)
	  (transpile `(lambda ,args ,@body) env)))

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
	  (transpile-same-scope-without-return body env)))

(define-lua-syntax (lua-ifor (key value expr) . body) env
  (format #f "(function()\nfor ~a,~a in ipairs(~a) do\n~a\nend\nend)()"
	  (true-name key)
	  (true-name value)
	  (transpile expr env)
	  (transpile-same-scope-without-return body env)))

(define-lua-syntax (while condition . body) env
  (format #f "(function()\nwhile(~a)do\n~a\nend\nend)()"
	  (transpile condition env)
	  (transpile-same-scope-without-return body env)))

(define (mappend f . args-list)
  (apply append (apply map f args-list)))

(define (var? expr)
  (symbol? expr))

(define (defined-var? expr env)
  (and (var? expr)
       (find-if (lambda (x)
		  (eq? (car x) expr))
		env)))

(define (true-name var :optional (env ()))
  (define (conv c)
    (cond ((eq? c #\-) "_UNDER_")
	  ((eq? c #\?) "_QUESTION_")
	  (else (list->string (list c)))))
  (list->string
   (mappend string->list
	    (map conv
		 (string->list
		  (format #f "~a~a"
			  (let ((x (find-if (lambda (x)
					      (eq? (car x) var))
					    env)))
			    (if x (cdr x) ""))
			  var))))))

(define (undefined-var? expr env)
  (and (var? expr)
       (not (find-if (lambda (x)
		       (eq? (car x) expr))
		     env))))

(define (transpile expr env)
  (cond ((defined-var? expr env)
	 (true-name expr env))
	((undefined-var? expr env)
	 (format #f "~a" (true-name expr)))
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

(define (transpile-same-scope-without-return exprs env)
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
	      (join-string
	       "\n_=nil\n"
	       (map (lambda (expr) (transpile expr env))
		    exprs))))))

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

