(define (distinct? items)
  (cond ((null? items) true)
        ((null? (cdr items)) true)
        ((member (car items) (cdr items)) false)
        (else (distinct? (cdr items)))))


(define (baker l) (car l))
(define (cooper l) (cadr l))
(define (fletcher l) (caddr l))
(define (miller l) (cadddr l))
(define (smith l) (cadddr (cdr l)))

(define (combine l1 l2) (append-map (lambda (x) (map (lambda (y) (cons x y)) l2)) l1)) 

(define possible-solutions (combine '(1 2 3 4)
				    (combine '(2 3 4 5)
					     (combine '(2 3 4)
						      (combine '(3 4 5) 
							       (combine '(1 2 3 4 5) '(())))))))
(define (is-solution? s)
  (and (> (miller s) (cooper s))
       (not (= (abs (- (fletcher s) (cooper s))) 1))
       (not (= (abs (- (smith s) (fletcher s))) 1))
       (distinct? s)))

(define solution (filter is-solution? possible-solutions))


