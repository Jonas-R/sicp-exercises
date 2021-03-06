;; generic arithmetic with simplified results

(load "ex2.79.scm")

;; raise
(define (raise-num i) (make-rational i 1))
(define (raise-rat r) (make-complex-from-real-imag (exact->inexact (/ (numer r) (denom r))) 0))

(put 'raise '(scheme-number) raise-num)
(put 'raise '(rational) raise-rat)

(define (raise x) (apply-generic 'raise x))

(define (numer x) (car x))
(define (denom x) (cdr x))


;; project
(define (project-rat r) (numer r))
(define (project-complex z) (make-rational (real-part z) 1))

(put 'project '(rational) project-rat)
(put 'project '(complex) project-complex)

(define (project x) (apply-generic 'project x))

(define (drop n)
  (if (not (null? (get 'project (list (type-tag n)))))
      (if (equ? n (raise (project n)))
	  (drop (project n))
	  n)
  n))


;; apply-generic for an arbitrary number of arguments, coerces using raise
(define (apply-generic op . args)
  (let ((type-tags (map type-tag args)))
    (let ((proc (get op type-tags)))
      (if (not (null? proc))
	  (if (equal? op 'raise)
	      (apply proc (map contents args))
	      (drop (apply proc (map contents args))))
	  (let* ((coerced-args (try-coercion type-tags args))
		(coerced-proc (get op (map type-tag coerced-args))))
	    (if (not (null? coerced-proc))
		(if (equal? op 'raise)
		    (apply coerced-proc (map contents coerced-args))
		    (drop (apply coerced-proc (map contents coerced-args))))
		(error "No method for these types"
		       (list op type-tags))))))))

;; try to coerce all args to a common type
;; returns a list of coerced args or #f if coercion not possible
(define (try-coercion types args)
  (define (helper i)
    (if (>= i (length types))
	#f
	(let ((coercion-type (list-ref types i)))
	  (if (can-coerce types args coercion-type)
	      (coerce-all types args coercion-type)
	      (helper (+ i 1))))))
  (helper 0))      
  

;; coerce all args to coercion-type, using successive raises
(define (coerce-all types args coercion-type)
  (define (coerce type arg)
    (if (not (equal? type coercion-type))
	(let ((new-arg (raise arg)))
	  (coerce (type-tag new-arg) new-arg))
	arg))
  (map coerce types args))

;; true iff all types can be coerced to coercion-type
(define (can-coerce types args coercion-type)
  (define (helper type arg)
    (if (equal? type coercion-type)
	#t
	(if (not (null? (get 'raise (list type))))
	    (helper (type-tag (raise arg)) (raise arg))
	    #f)))
  (define (and-helper x y)
    (and x y))
  (fold-right and-helper #t (map helper types args)))

