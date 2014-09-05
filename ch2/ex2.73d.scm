(load "symbolic_differentiation.scm")

;; symbolic differentiation using data-directed programming

(define (deriv exp var)
   (cond ((number? exp) 0)
         ((variable? exp) (if (same-variable? exp var) 1 0))
         (else ((get (operator exp) 'deriv) (operands exp)
                                            var))))
(define (operator exp) (car exp))
(define (operands exp) (cdr exp))

(define (install-package)
  ;; internal procedures
  (define (deriv-sum exp var)
    (make-sum (deriv (addend exp) var)
	      (deriv (augend exp) var)))
  (define (deriv-product exp var)
    (make-sum
      (make-product (multiplier exp)
		    (deriv (multiplicand exp) var))
      (make-product (deriv (multiplier exp) var)
		    (multiplicand exp))))
  (define (deriv-exponent exp var)
     (make-product
	  (make-product (exponent exp)
			(make-exponentiation (base exp) (make-sum (exponent exp) '-1)))
	  (deriv (base exp) var)))
  ;; interface to rest of system
  (put '+ 'deriv deriv-sum)
  (put '* 'deriv deriv-product)
  (put '** 'deriv deriv-exponent)
  
  (display "done"))
  
;; modified accessors because new deriv only passes operands
(define (addend s) (car s))
(define (augend s) (if (= (length (cdr s)) 1) (cadr s) (apply make-sum (cdr s))))
(define (multiplier p) (car p))
(define (multiplicand p) (if (= (length (cdr p)) 1) (cadr p) (apply make-product (cdr p))))
(define (base e) (car e))
(define (exponent e) (cadr e))
  
;; dispatch table implementation
(define dispatch-table (make-equal-hash-table))

(define (put type-tag op function)
  (hash-table/put! dispatch-table (list op type-tag) function))

(define (get type-tag op)
  (hash-table/get dispatch-table (list op type-tag) '()))

(install-package)
