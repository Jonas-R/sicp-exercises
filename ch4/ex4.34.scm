;; load the lazy evaluator before using this

(define (user-print object)
  (cond ((compound-procedure? object)
	 (display (list 'compound-procedure
			(procedure-parameters object)
			(procedure-body object)
			'<procedure-env>)))
	((pair? object) (print-list list))
	(else (display object))))

(define (print-list l)
  (define (helper l n)
    (cond ((null? l) (newline))
	  ((= n 0) (begin (display "...") (newline))) 
	  (else (display (car l) (helper (cdr l) (- n 1))))))
  (helper l 20))
