;; test-recipient syntax for cond clauses

(define (test-recipient-clause? clause) (eq? (cadr clause) '=>))
(define (cond-recipient clause) (caddr clause))

(define (expand-clauses clauses)
  (if (null? clauses)
      'false                          ; no else clause
      (let ((first (car clauses))
            (rest (cdr clauses)))
        (if (cond-else-clause? first)
            (if (null? rest)
                (sequence->exp (cond-actions first))
                (error "ELSE clause isn't last -- COND->IF"
                       clauses))
	    (if (test-recipient-clause? first)
		(make-if (cond-predicate first)
			 (list (cond-recipient first) (cond-predicate first))
			 (expand-clauses rest))
		(make-if (cond-predicate first)
			 (sequence->exp (cond-actions first))
			 (expand-clauses rest)))))))

;; add assoc and cadr to primitives for testing
(define primitive-procedures
  (list (list 'car car)
        (list 'cdr cdr)
        (list 'cons cons)
        (list 'null? null?)
	(list '+ +)
	(list '* *)
	(list '- -)
	(list '/ /)
	(list 'assoc assoc)
	(list 'cadr cadr)
        ))

;; (cond ((assoc 'b '((a 1) (b 2))) => cadr) (else false))
