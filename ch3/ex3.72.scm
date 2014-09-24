(load "ex3.70.scm")
					   
(define (weight-func pair)
  (let ((i (car pair))
	(j (cadr pair)))
    (+ (expt i 2) (expt j 2))))

(define (squares-filter s)
  (if (= (weight-func (stream-car s))
	 (weight-func (stream-car (stream-cdr s)))
	 (weight-func (stream-car (stream-cdr (stream-cdr s)))))
      (cons-stream (list (+ (expt (car (stream-car s)) 2) (expt (cadr (stream-car s)) 2))
			 (stream-car s)
			 (stream-car (stream-cdr s))
			 (stream-car (stream-cdr (stream-cdr s))))
		   (squares-filter (stream-cdr s)))
      (squares-filter (stream-cdr s))))

;; integers that can be written as the sum of two squares in at least three ways
(define squares-in-three-ways (squares-filter
			       (pairs-weighted integers 
					       integers
					       weight-func)))
