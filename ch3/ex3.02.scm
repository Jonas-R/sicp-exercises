;; monitor calls to a function

(define (make-monitored f)
  (let ((num-calls 0))
	(define (mf arg)
	  (cond ((eq? arg 'how-many-calls?) num-calls)
		((eq? arg 'reset-count) (begin (set! num-calls 0)
					       num-calls))
		(else (begin (set! num-calls (+ num-calls 1))
			     (f arg)))))
	mf))
