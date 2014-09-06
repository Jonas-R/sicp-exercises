(load "generic_arithmetic.scm")

;; apply-generic for an arbitrary number of arguments
(define (apply-generic op . args)
  (let ((type-tags (map type-tag args)))
    (let ((proc (get op type-tags)))
      (if (not (null? proc))
          (apply proc (map contents args))
	  (let* ((coerced-args (try-coercion type-tags args))
		(coerced-proc (get op (map type-tag coerced-args))))
	    (if (not (null? coerced-proc))
		(apply coerced-proc (map contents coerced-args)) 
		(error "No method for these types"
		       (list op type-tags))))))))

;; try to coerce all args to a common type
;; returns a list of coerced args or #f if coercion not possible
(define (try-coercion types args)
  (define (helper i)
    (if (>= i (length types))
	#f
	(let ((coercion-type (list-ref types i)))
	  (if (can-coerce types coercion-type)
	      (coerce-all types args coercion-type)
	      (helper (+ i 1))))))
  (helper 0))      
  

;; coerce all args to coercion-type
(define (coerce-all types args coercion-type)
  (define (coerce type arg)
    (if (not (equal? type coercion-type))
	((get-coercion type coercion-type) arg)
	arg))
  (map coerce types args))

;; true iff all types can be coerced to coercion-type
(define (can-coerce types coercion-type)
  (define (helper type)
    (or (equal? type coercion-type) 
	((lambda (x) (if (null? x) #f x)) (get-coercion type coercion-type))))
  (define (and-helper x y)
    (and x y))
  (fold-right and-helper #t (map helper types)))
