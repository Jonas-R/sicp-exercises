(load "digital_circuits.scm")

;; or-gate in terms of and-gate and inverter
(define (or-gate a1 a2 output)
  (let ((t1 (make-wire))
	(t2 (make-wire))
	(t3 (make-wire)))
  (inverter a1 t1)
  (inverter a2 t2)
  (and-gate t1 t2 t3)
  (inverter t3 output)))
