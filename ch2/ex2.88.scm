(load "symbolic_algebra.scm")

(define (install-negation)
  ;; internal procedures
  (define (negation-scheme-number n) (- n))
  (define (negation-rational r) (make-rational (- (numer r)) (denom r)))
  (define (negation-complex z) (make-complex-from-real-imag (- (real-part z)) (- (imag-part z))))
  (define (negation-poly p) 
    (make-polynomial
     (variable p)
     (map
      (lambda (term) (make-term (order term) (neg (coeff term))))
      (term-list p))))

  ;; interface to rest of the system
  (put 'neg '(scheme-number) negation-scheme-number)
  (put 'neg '(rational) negation-rational)
  (put 'neg '(complex) negation-complex)
  (put 'neg '(polynomial) negation-poly))

(define (neg x) (apply-generic 'neg x))
(define (subtract x y) (add x (neg y)))

(define (term-list p) (cdr p))
(define (numer r) (car r))
(define (denom r) (cdr r))
(define (variable p) (car p))

(install-negation)
