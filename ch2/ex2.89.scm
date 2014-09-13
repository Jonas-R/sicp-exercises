(load "symbolic_algebra")

;; representation for dense termlists
(define (adjoin-term term term-list)
  (if (=zero? (coeff term))
      term-list
      (if (>= (order term) (length term-list))
	  (adjoin-term term (cons 0 term-list))
	  (list-set! term-list
		     (- (length term-list) (order term) 1)
		     (coeff term)))))

(define (the-empty-termlist) '())
(define (first-term term-list) 
  (make-term (- (length term-list) 1)
	     (car term-list)))
(define (rest-terms term-list) (cdr term-list))
(define (empty-termlist? term-list) (null? term-list))


(define (list-set! list i value)
  (if (= i 0)
      (cons value (cdr list))
      (cons (car list) (list-set! (cdr list) (- i 1) value))))
