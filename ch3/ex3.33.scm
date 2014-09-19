(load "constraint_propagation.scm")

(define (averager a b c)
  (let ((sum (make-connector))
	(onehalf (make-connector)))
    (adder a b sum)
    (constant 0.5 onehalf)
    (multiplier sum onehalf c)))
