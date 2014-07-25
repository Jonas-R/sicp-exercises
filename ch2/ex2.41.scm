(load "enumerate.scm")

; does scheme not have currying???
(define (sum? s)
  (lambda (l)
    (= s (fold-right + 0 l))))

(define (triples n)
  (flatmap (lambda (i)
	     (flatmap (lambda (j)
		    (map (lambda (k) (list i j k))
			 (enumerate-interval 1 (- j 1))))
		  (enumerate-interval 1 (- i 1))))
	   (enumerate-interval 1 n)))

; find ordered triples of integers <= n with sum s
(define (triples-sum n s)
  (filter (sum? s) (triples n)))
  
