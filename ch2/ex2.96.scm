(load "ex2.93.scm")

(define (gcd-terms a b)
  (if (empty-termlist? b)
      a
      (let* ((terms (gcd-terms b (pseudoremainder-terms a b)))
	     (coeff-gcd (apply gcd (map coeff terms)))
	     (reduced-terms (map (lambda (t) (make-term (order t)
							(/ (coeff t) coeff-gcd)))
				 terms)))
	reduced-terms)))

(define (pseudoremainder-terms a b) 
  (let* ((t1 (first-term a))
	 (t2 (first-term b))
	 (fac (expt (coeff t2) (+ 1 (order t1) (- (order t2)))))
	 (new-a (map (lambda (term) (make-term (order term)
					       (* fac (coeff term))))
		     a)))
    (cadr (div-terms new-a b))))

(define (gcd-poly p1 p2)
  (if (same-variable? (variable p1) (variable p2))
      (make-polynomial (variable p1)
		       (gcd-terms (term-list p1)
				  (term-list p2)))
      (error "Polys not in same var -- GCD-POLY"
	     (list p1 p2))))

(put 'greatest-common-divisor '(polynomial polynomial) gcd-poly)
(put 'greatest-common-divisor '(scheme-number scheme-number)
    (lambda (a b) (gcd a b)))
(define (greatest-common-divisor a b)
  (apply-generic 'greatest-common-divisor a b))
