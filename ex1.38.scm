;;load cont-frac procedure
(load "ex1.37.scm")

;;approximate e
(define (approx-e)
  (+ 2 (cont-frac (lambda (i) 1.0)
		  (lambda (i) (cond ((= (modulo (- i 5) 3) 3) (* (/ (+ i 1) 3) 2))
				    (else 1.0)))
		  100)))
