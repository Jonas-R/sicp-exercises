(load "ex3.61.scm")

(define (div-series s1 s2)
  (mul-series s1 (invert-unit-series (scale-stream (/ 1 (stream-car s2))
						   s2))))
