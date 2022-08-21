(use util.match)

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

(define (find-if f list)
  (if (null? list)
      #f
      (let ((v (f (car list))))
	(if v (car list) (find-if f (cdr list))))))

(define (join-string sep strings)
  (define (rec lists)
    (if (null? lists)
	""
	(format #f "~a~a~a" sep (car lists) (rec (cdr lists)))))
  (if (null? strings)
      ""
      (format #f "~a~a" (car strings) (rec (cdr strings)))))

(define (mappend f . args-list)
  (apply append (apply map f args-list)))

(define (remove-last list)
  (reverse (cdr (reverse list))))

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
	    (lambda (expr env)
	      (match expr
		     (('head . args)
		      (some then))
		     (xs (none))))
	    *lua-transpile-macros*)))))

(define-lua-syntax (require path) env
  (format #f "(require ~a)" (transpile path env)))

(define-lua-syntax (set! var expr) env
  (transpile
   `(begin
      (lua-eval ,(format #f "~a = ~a" (transpile var env) (transpile expr env)))
      ,var)
   env))

(define-lua-syntax (begin . body) env
  (transpile `(let () ,@body)
	     env))

(define-lua-syntax (let variables . body) env
  (transpile `((lambda ,(map car variables) ,@body) ,@(map cadr variables))
	     env))

(define-lua-syntax (lambda args . body) env
  (let ((normal-lambda
	 (lambda ()
	   (format #f "function(~a)\n~a\nend"
		   (join-string ", " (map to-lua-symbol args))
		   (transpile-same-scope
		    body
		    (add-env-binds env (map (lambda (x) (cons x x)) args))))))
	(variadib-lambda
	 (lambda ()
	   (let ((normal-args (reverse (reverse args)))
		 (variadic-arg (cdr (last-pair args))))
	     (format #f "function(~a,...)\nlocal ~a = ~a({...})\n~a\nend"
		     (join-string "," (map to-lua-symbol normal-args))
		     (to-lua-symbol variadic-arg)
		     (transpile 'array-to-list env)
		     (transpile-same-scope
		      body
		      (add-env-binds
		       env
		       (append (map (lambda (x) (cons x x)) normal-args)
			       (list (cons variadic-arg variadic-lambda)))))))))
	(only-variadic-lambda
	 (lambda ()
	   (format #f "function(...)\nlocal ~a = ~a({...})\n~a\nend"
		   (to-lua-symbol args)
		   (transpile 'array-to-list env)
		   (transpile-same-scope
		    body
		    (add-env-bind env args args))))))
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
		  (format #f "~a = ~a" (to-lua-symbol (car c)) (transpile (cadr c) env)))
		binds))))

(define-lua-syntax (make-array . exprs) env
  (format #f "{~a}"
	  (join-string
	   ","
	   (map (lambda (expr)
		  (transpile expr env))
		exprs))))

(define-lua-syntax (if condition then else) env
  (format #f "(function() if ~a then \nreturn ~a\nelse\nreturn ~a\nend\nend)()"
	  (transpile condition env)
	  (transpile then env)
	  (transpile else env)))

(define-lua-syntax (cond (condition then) . other) env
  (transpile `(if ,condition ,then (cond ,@other)) env))

(define-lua-syntax (cond) env
  (transpile `(error "lisp cond error: failed all conditions") env))

(define-lua-syntax (cond ('else then)) env
  (transpile then env))

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

(define-syntax define-compare-operator
  (syntax-rules ()
    ((_ symbol op)
     (define-lua-syntax (symbol . args) env
       (let ((exprs (map (lambda (expr) (transpile expr env)) args)))
	 (format #f "(~a)"
		 (join-string
		  " and "
		  (map (lambda (l r) (format #f "~a ~a ~a" l op r))
		       exprs (cdr exprs)))))))))

(define-compare-operator < "<")
(define-compare-operator > ">")
(define-compare-operator <= "<=")
(define-compare-operator >= ">=")
(define-compare-operator = "==")
(define-compare-operator /= "~=")

(define-lua-syntax (not x) env
  (format #f "(not ~a)" (transpile x env)))


(define-lua-syntax (lua-for (key value expr) . body) env
  (format #f "(function()\nfor ~a,~a in pairs(~a) do\n~a\nend\nend)()"
	  (to-lua-symbol key)
	  (to-lua-symbol value)
	  (transpile expr env)
	  (transpile `(begin ,@body)
		     (add-env-binds
		      env
		      (list (cons key key)
			    (cons value value))))))

(define-lua-syntax (lua-ifor (key value expr) . body) env
  (format #f "(function()\nfor ~a,~a in ipairs(~a) do\n~a\nend\nend)()"
	  (to-lua-symbol key)
	  (to-lua-symbol value)
	  (transpile expr env)
	  (transpile `(begin ,@body)
		     (add-env-binds
		      env
		      (list (cons key key)
			    (cons value value))))))

(define-lua-syntax (while condition . body) env
  (format #f "(function()\nwhile(~a)do\n~a\nend\nend)()"
	  (transpile condition env)
	  (transpile `(begin ,@body) env)))

(define-lua-syntax (lua-eval code) env
  code)

(define (var? expr)
  (symbol? expr))

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

(define (add-env-bind env symbol full-name)
  (cons (cons symbol full-name) env))

(define (add-env-binds env binds)
  (if (null? binds)
      env
      (let ((symbol (caar binds))
	    (full-name (cdar binds)))
	(add-env-binds
	 (add-env-bind env symbol full-name)
	 (cdr binds)))))

(define (transpile-nil expr env)
  (if (null? expr)
      (some "nil")
      (none)))

(define (transpile-var expr env)
  (if (var? expr)
      (some (true-name expr env))
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
	      transpile-var
	      transpile-number
	      transpile-string
	      transpile-macros
	      transpile-function-call))))

(define (transpile-same-scope exprs env)
  (define current-objects-root (next-objects-root))
  (define new-definitions
    (mappend (match-lambda (('define (var . _) . _) (list var))
			   (('define var _) (list var))
			   (_ ()))
	     exprs))
  (define new-bindings
    (map (lambda (symbol)
	   (cons symbol (format #f "~a.~a" current-objects-root symbol)))
	 new-definitions))
  (format #f "local ~a = {}\n~a\n"
	  current-objects-root
	  (let* ((new-env (add-env-binds env new-bindings))
		 (evaled (map (lambda (expr)
				(format #f "~a;" (transpile expr new-env)))
			      exprs)))
	    (join-string
	     "\n"
	     (append (remove-last evaled)
		     (list (format #f "return ~a" (last evaled))))))))


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

(define (evaluate-transpiler-level-eval program)
  (let ((targets (filter (match-lambda
			  (('transpiler-eval . _) #t)
			  (_ #f))
			 program)))
    (eval `(begin ,@(mappend cdr targets)) (interaction-environment))))

(define (remove-transpiler-level-eval program)
  (filter (match-lambda
	   (('transpiler-eval . _) #f)
	   (_ #t))
	  program))

(define (read-while-eof)
  (let ((res (read)))
    (if (eof-object? res)
	()
	(cons res (read-while-eof)))))

(let ((program (append global-programs (read-while-eof))))
  (evaluate-transpiler-level-eval program)
  (display (transpile-same-scope
	    (remove-transpiler-level-eval program)
	    ())))
