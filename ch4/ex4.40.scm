;; load this into amb evaluator
;; there are 5 solutions to this modified problem

(define (distinct? items)
  (cond ((null? items) true)
        ((null? (cdr items)) true)
        ((member (car items) (cdr items)) false)
        (else (distinct? (cdr items)))))

(define (multiple-dwelling)
  (let ((cooper (amb 2 3 4 5))
        (miller (amb 3 4 5)))
    (require (> miller cooper))
    (let ((fletcher (amb 2 3 4)))
      (require (not (= (abs (- fletcher cooper)) 1)))
      (require (distinct? (list cooper miller fletcher)))
      (let ((baker (amb 1 2 3 4))
	    (smith (amb 1 2 3 4 5)))
	(require (not (= (abs (- smith fletcher)) 1)))
	(require
	 (distinct? (list baker cooper fletcher miller smith)))    
	(list (list 'baker baker)
	      (list 'cooper cooper)
	      (list 'fletcher fletcher)
	      (list 'miller miller)
	      (list 'smith smith))))))

(define (require p)
  (if (not p) (amb)))
