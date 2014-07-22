(define (same-parity n . rest)
  (define (same-parity-helper acc rest)
    (if (null? rest)
	acc
	(if (= (modulo (car rest) 2) (modulo (car acc) 2))
	    (same-parity-helper (append acc (list (car rest))) (cdr rest))
	    (same-parity-helper acc (cdr rest)))))
  (same-parity-helper (list n) rest))
