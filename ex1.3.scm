(define (sum-of-largest-squares a b c) (cond ((and (> a c) (> b c) (+ (expt a 2) (expt b 2))))
			       	      	    ((and (> a b) (> c b) (+ (expt a 2) (expt c 2))))
					    (else      	       	  (+ (expt b 2) (expt c 2)))))
