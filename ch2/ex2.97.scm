(load "ex2.96.scm")

(define (reduce-terms n d)
    (let*  ((term-gcd (gcd-terms n d))
            (o1 (max  (order (first-term n))
                      (order (first-term d))))
            (o2 (order (first-term term-gcd)))
            (c (coeff (first-term term-gcd)))
            (fac (expt c (+ 1 o1 (- o2))))
            (new-i (map (lambda (t) (make-term (order t)
					       (* (coeff t) fac))) 
			n))
            (new-d (map (lambda (t) (make-term (order t)
					       (* (coeff t) fac)))
			d))
            (nn (quotient-terms new-i term-gcd))
            (dd (quotient-terms new-d term-gcd)))
      (list nn dd)))

(define (quotient-terms a b)
    (car (div-terms a b)))

(define (reduce-poly p1 p2)
    (if (same-variable? (variable p1) (variable p2))
      (let ((result (reduce-terms
                      (term-list p1)
                      (term-list p2))))
        (cons
          (make-polynomial (variable p1) (car result))
          (make-polynomial (variable p2) (cadr result))))
      (error "Polys not in same var -- REDUCE-POLY" 
        (list p1 p2))))

(define (reduce-integers n d)
  (let ((g (gcd n d)))
    (list (/ n g) (/ d g))))

(put 'reduce '(scheme-number scheme-number)
  (lambda (n d) (reduce-integers n d)))

(define (reduce n d) (apply-generic 'reduce n d))

(put 'reduce '(polynomial polynomial) reduce-poly)


(define (install-rational-package)
  ;; internal procedures
  (define (numer x) (car x))
  (define (denom x) (cdr x))
  (define (make-rat n d) (reduce n d))
  (define (add-rat x y)
    (make-rat (add (mul (numer x) (denom y))
		   (mul (numer y) (denom x)))
              (mul (denom x) (denom y))))
  (define (sub-rat x y)
    (make-rat (sub (mul (numer x) (denom y))
		   (mul (numer y) (denom x)))
              (mul (denom x) (denom y))))
  (define (mul-rat x y)
    (make-rat (mul (numer x) (numer y))
              (mul (denom x) (denom y))))
  (define (div-rat x y)
    (make-rat (mul (numer x) (denom y))
              (mul (denom x) (numer y))))
  ;; interface to rest of the system
  (define (tag x) (attach-tag 'rational x))
  (put 'add '(rational rational)
       (lambda (x y) (tag (add-rat x y))))
  (put 'sub '(rational rational)
       (lambda (x y) (tag (sub-rat x y))))
  (put 'mul '(rational rational)
       (lambda (x y) (tag (mul-rat x y))))
  (put 'div '(rational rational)
       (lambda (x y) (tag (div-rat x y))))

  (put 'make 'rational
       (lambda (n d) (tag (make-rat n d))))
  'done)

(define (make-rational n d)
  ((get 'make 'rational) n d))


(install-rational-package)
