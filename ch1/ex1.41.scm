;;double takes a procedure with one argument and returns a procedure that applies the argument twice
(define (double f)
  (lambda (x) (f (f x))))
