(load "ex3.59.scm")

(define (mul-series s1 s2)
  (cons-stream (* (stream-car s1) (stream-car s2))
	       (add-streams (scale-stream (stream-car s1) (stream-cdr s2))
			    (mul-series (stream-cdr s1) s2))))

(define (scale-stream x s) (stream-map (lambda (el) (* x el)) s))
(define (add-streams s1 s2) (stream-map + s1 s2))
