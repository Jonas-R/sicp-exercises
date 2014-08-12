(define (deriv exp var)
  (cond ((number? exp) 0)
        ((variable? exp)
         (if (same-variable? exp var) 1 0))
        ((sum? exp)
         (make-sum (deriv (addend exp) var)
                   (deriv (augend exp) var)))
        ((product? exp)
         (make-sum
           (make-product (multiplier exp)
                         (deriv (multiplicand exp) var))
           (make-product (deriv (multiplier exp) var)
                         (multiplicand exp))))
	((exponentiation? exp)
	 (make-product
	  (make-product (exponent exp)
			(make-exponentiation (base exp) (make-sum (exponent exp) '-1)))
	  (deriv (base exp) var)))
        (else
         (error "unknown expression type -- DERIV" exp))))

;; representing algebraic expressions

(define (variable? x) (symbol? x))

(define (same-variable? v1 v2)
  (and (variable? v1) (variable? v2) (eq? v1 v2)))

(define (sum? x)
  (and (pair? x) (eq? (car x) '+)))

(define (addend s) (cadr s))

(define (augend s) (if (= (length (cddr s)) 1) (caddr s) (apply make-sum (cddr s))))

; exercise 2.57
(define (make-sum a1 a2 . rest) 
  (cond ((=number? a1 0) 
	 (if (null? rest) a2 (apply make-sum a2 rest)))
	((=number? a2 0) 
	 (if (null? rest) a1 (apply make-sum a1 rest)))
	((and (number? a1) (number? a2)) 
	 (if (null? rest) (+ a1 a2) (apply make-sum (+ a1 a2) rest)))
	(else 
	 (if (null? rest) (list '+ a1 a2) (apply list '+ a1 a2 rest)))))

(define (product? x)
  (and (pair? x) (eq? (car x) '*)))

(define (multiplier p) (cadr p))

(define (multiplicand p) (if (= (length (cddr p)) 1) (caddr p) (apply make-product (cddr p))))

(define (make-product m1 m2 . rest) 
  (cond ((or (=number? m1 0) (=number? m2 0)) 0)
	((=number? m1 1) 
	 (if (null? rest) m2 (apply make-product m2 rest)))
	((=number? m2 1) 
	 (if (null? rest) m1 (apply make-product m1 rest)))
	((and (number? m1) (number? m2)) 
	 (if (null? rest) (* m1 m2) (apply make-product (* m1 m2) rest)))
	(else 
	 (if (null? rest) (list '* m1 m2) (apply list '* m1 m2 rest)))))

(define (=number? exp num)
  (and (number? exp) (= exp num)))

;; exercise 2.56
(define (exponentiation? x)
  (and (pair? x) (eq? (car x) '**)))

(define (base e) (cadr e))

(define (exponent e) (caddr e))

(define (make-exponentiation b e)
  (cond ((=number? e 0) 1)
	((=number? e 1) b)
	(else (list '** b e))))
