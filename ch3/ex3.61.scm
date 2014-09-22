(load "ex3.60.scm")

(define (invert-unit-series s)
  (cons-stream 1 (stream-map (lambda (x) (- x))
			     (mul-series (stream-cdr s)
					 (invert-unit-series s)))))
