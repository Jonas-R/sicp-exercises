;; accumulator

(define (make-accumulator val)
  (lambda (inc)
    (begin (set! val (+ val inc))
	   val)))
