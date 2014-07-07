(load "ex1.37.scm")

;; tangent as continued fraction
(define (tan-cf x k)
  (cont-frac (lambda (i) (if (= i 1) x (- 0 (* x x))))
	     (lambda (i) (- (* 2 i) 1.0))
	     k))
