;; load symbolic algebra with negation
(load "ex2.88.scm")

;; divide polynomials

(define (div-terms L1 L2)
  (if (empty-termlist? L1)
      (list (the-empty-termlist) (the-empty-termlist))
      (let ((t1 (first-term L1))
            (t2 (first-term L2)))
        (if (> (order t2) (order t1))
            (list (the-empty-termlist) L1)
            (let ((new-c (div (coeff t1) (coeff t2)))
                  (new-o (- (order t1) (order t2))))
              (let ((rest-of-result
		     (div-terms (add-terms L1
				     (cddr (neg (make-polynomial 'x (mul-terms L2 (adjoin-term (make-term new-o new-c) (the-empty-termlist)))))))
			  L2)))
		(list 
		 (cons (make-term new-o new-c) (car rest-of-result))
		 (cadr rest-of-result))))))))


(define (div-poly p1 p2)
    (if (same-variable? (variable p1) (variable p2))
	(let ((res (div-terms (term-list p1) (term-list p2))))
	  (list (make-polynomial (variable p1) (car div-result))
		(make-polynomial (variable p1) (cadr div-result))))
	(error "Polys not in same var -- DIV-POLY"
	       (list p1 p2))))

(define (tag p) (attach-tag 'polynomial p))
(put 'div '(polynomial polynomial) 
       (lambda (p1 p2) (tag (div-poly p1 p2))))

(define (same-variable? v1 v2)
  (and (variable? v1) (variable? v2) (eq? v1 v2)))
(define (variable? x) (symbol? x))
