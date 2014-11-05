;; load this into amb evaluator
;; solution: ((betty 3) (ethel 5) (joan 2) (kitty 1) (mary 4))

(define (distinct? items)
  (cond ((null? items) true)
        ((null? (cdr items)) true)
        ((member (car items) (cdr items)) false)
        (else (distinct? (cdr items)))))

(define (schoolgirl-problem)
  (let ((betty (amb 1 2 3 4 5))
        (ethel (amb 1 2 3 4 5))
	(joan (amb 1 2 3 4 5))
	(kitty (amb 1 2 3 4 5))
	(mary (amb 1 2 3 4 5)))
    (require (not (eq? (= kitty 2) (= betty 3))))
    (require (not (eq? (= ethel 1) (= joan 2))))
    (require (not (eq? (= joan 3) (= ethel 5))))
    (require (not (eq? (= kitty 2) (= mary 4))))
    (require (not (eq? (= mary 4) (= betty 1))))
    (require (distinct? (list betty ethel joan kitty mary)))
    (list (list 'betty betty)
	  (list 'ethel ethel)
	  (list 'joan joan)
	  (list 'kitty kitty)
	  (list 'mary mary))))

(define (require p)
  (if (not p) (amb)))