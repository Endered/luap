(transpiler-eval
 (defmacro (when condition . then)
   `(if ,condition (begin ,@then) ()))
 )


(when (< 1 2)
  (print "1 is less than 2")
  (print "and this is when macros"))
