(load "digital_circuits.scm")

;; ripple carry adder
(define (ripple-carry-adder as bs ss c)
  (if (null? as)
      '()
      (let ((cout (make-wire)))
	(full-adder (car as) (car bs) c (car ss) cout)
	(ripple-carry-adder (cdr as) (cdr bs) (cdr ss) cout))))
