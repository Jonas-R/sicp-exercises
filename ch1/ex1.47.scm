;;load fixed-point and repeated
(load "fixed_point.scm")
(load "ex1.43.scm")

(define (average-damp f)
  (lambda (x) (/ (+ x (f x)) 2)))

;;log with arbitrary base (is this really not built-in?!)
(define (log-base x base)
  (/ (log x) (log base)))

;;finding n-th roots using fixed-point
(define (n-th-root x n)
  (fixed-point ((repeated average-damp (floor (log-base n 2))) (lambda (y) (/ x (expt y (- n 1)))))
	       1.0))
