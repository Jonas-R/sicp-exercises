;; import symbolic arithmetic with =zero? function
(load "ex2.80.scm")

;; symbolic algebra with two term list representations

(define (install-polynomial-package)
  ;; internal procedures
  ;; representation of poly
  (define (make-poly variable term-list)
    (cons variable term-list))
  (define (variable p) (car p))
  (define (term-list p) (cdr p))
 
  (define (variable? x) (symbol? x))

  (define (same-variable? v1 v2)
    (and (variable? v1) (variable? v2) (eq? v1 v2)))

  (define (add-poly p1 p2)
    (if (same-variable? (variable p1) (variable p2))
	(make-poly (variable p1)
		   (add-terms (term-list p1)
			      (term-list p2)))
	(error "Polys not in same var -- ADD-POLY"
	       (list p1 p2))))

  (define (mul-poly p1 p2)
    (if (same-variable? (variable p1) (variable p2))
	(make-poly (variable p1)
		   (mul-terms (term-list p1)
			      (term-list p2)))
	(error "Polys not in same var -- MUL-POLY"
	       (list p1 p2))))

  ;; exercise 2.87
  (define (=zero? poly)
    (define (and-helper x y) (and x y))
    (or (null? (term-list poly)) 
	(fold-right and-helper #t (map (lambda (x) (= (coeff x) 0)) (term-list poly)))))

  ;; interface to rest of the system
  (define (tag p) (attach-tag 'polynomial p))
  (put 'add '(polynomial polynomial) 
       (lambda (p1 p2) (tag (add-poly p1 p2))))
  (put 'mul '(polynomial polynomial) 
       (lambda (p1 p2) (tag (mul-poly p1 p2))))

  (put 'make 'polynomial
       (lambda (var terms) (tag (make-poly var terms))))
  (put 'zero '(polynomial) =zero?)
  'done)

(define (add-terms L1 L2)
  (cond ((empty-termlist? L1) L2)
        ((empty-termlist? L2) L1)
        (else
         (let ((t1 (first-term L1)) (t2 (first-term L2)))
           (cond ((> (order t1) (order t2))
                  (adjoin-term
                   (make-term (order t1) (coeff t1)) (add-terms (rest-terms L1) L2)))
                 ((< (order t1) (order t2))
                  (adjoin-term
                   (make-term (order t2) (coeff t2)) (add-terms L1 (rest-terms L2))))
                 (else
                  (adjoin-term
                   (make-term (order t1)
                              (add (coeff t1) (coeff t2)))
                   (add-terms (rest-terms L1)
                              (rest-terms L2)))))))))

(define (mul-terms L1 L2)
  (if (empty-termlist? L1)
      (the-empty-sparse-termlist)
      (add-terms (mul-term-by-all-terms (first-term L1) L2)
                 (mul-terms (rest-terms L1) L2))))

(define (mul-term-by-all-terms t1 L)
  (if (empty-termlist? L)
      (the-empty-sparse-termlist)
      (let ((t2 (first-term L)))
        (adjoin-term
         (make-term (+ (order t1) (order t2))
                    (mul (coeff t1) (coeff t2)))
         (mul-term-by-all-terms t1 (rest-terms L))))))


;; Representing term lists
(define (install-sparse-term-list)
  (define (adjoin-term term term-list)
    (if (=zero? (coeff term))
	term-list
	(cons term term-list)))

  (define (the-empty-termlist) '())
  (define (first-term term-list) (make-term 
				       (order (car term-list))
				       (coeff (car term-list))))
  (define (rest-terms term-list) (cdr term-list))
  (define (empty-termlist? term-list) (null? term-list))

  (define (make-term order coeff) (list order coeff))

  ;; interface to rest of the system
  (define (tag t) (attach-tag 'sparse-list t))
  (put 'first-term '(sparse-list) first-term)
  (put 'adjoin-term '(term sparse-list) 
       (lambda (t l) (tag (adjoin-term t l))))
  (put 'rest-terms '(sparse-list) 
       (lambda (l) (tag (rest-terms l))))
  (put 'empty-termlist? '(sparse-list) empty-termlist?)
  'done)

(define (install-dense-term-list)
  (define (adjoin-term term term-list)
  (if (=zero? (coeff term))
      term-list
      (if (>= (order term) (length term-list))
	  (adjoin-term term (cons 0 term-list))
	  (list-set! term-list
		     (- (length term-list) (order term) 1)
		     (coeff term)))))

  (define (first-term term-list) 
    (make-term (- (length term-list) 1)
	       (car term-list)))
  (define (rest-terms term-list) (cdr term-list))
  (define (empty-termlist? term-list) (null? term-list))

  (define (make-term order coeff) (list order coeff))

  ;; interface to rest of the system
  (define (tag t) (attach-tag 'dense-list t))
  (put 'first-term '(dense-list) 
       (lambda (l) (first-term l)))
  (put 'adjoin-term '(term dense-list) 
       (lambda (t l) (tag (adjoin-term t l))))
  (put 'rest-terms '(dense-list) 
       (lambda (l) (tag (rest-terms l))))
  (put 'empty-termlist? '(dense-list) empty-termlist?)
  'done)

(define (first-term term-list) (apply-generic 'first-term term-list))
(define (adjoin-term term term-list) (apply-generic 'adjoin-term term term-list))
(define (rest-terms term-list) (apply-generic 'rest-terms term-list))
(define (empty-termlist? term-list) (apply-generic 'empty-termlist? term-list))

(define (the-empty-sparse-termlist) '(sparse-list)) 
(define (the-empty-dense-termlist)  '(dense-list))

(define (make-term order coeff) (list 'term order coeff))
(define (order term) (car term))
(define (coeff term) (cadr term))

;; Constructor
(define (make-polynomial var terms)
  ((get 'make 'polynomial) var terms))

(define (list-set! list i value)
  (if (= i 0)
      (cons value (cdr list))
      (cons (car list) (list-set! (cdr list) (- i 1) value))))

(install-polynomial-package)
(install-sparse-term-list)
(install-dense-term-list)
