(load "ex2.93.scm")

(define (gcd-terms a b)
  (if (empty-termlist? b)
      a
      (gcd-terms b (remainder-terms a b))))

(define (remainder-terms a b) (cadr (div-terms a b)))

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
