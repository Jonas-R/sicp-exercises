(load "ex3.70.scm")
					   
(define (ramanujan-weight pair)
  (let ((i (car pair))
	(j (cadr pair)))
    (+ (expt i 3) (expt j 3))))

(define (ramanujan-filter s)
  (if (= (ramanujan-weight (stream-car s))
	 (ramanujan-weight (stream-car (stream-cdr s))))
      (cons-stream (+ (expt (car (stream-car s)) 3) (expt (cadr (stream-car s)) 3))
		   (ramanujan-filter (stream-cdr s)))
      (ramanujan-filter (stream-cdr s))))

(define ramanujan-numbers (ramanujan-filter
			   (pairs-weighted integers 
					   integers
					   ramanujan-weight)))
