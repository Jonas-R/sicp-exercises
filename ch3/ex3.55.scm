(define (add-streams s1 s2) (stream-map + s1 s2))

(define (partial-sums s)
  (cons-stream (stream-car s)
	       (add-streams (partial-sums s) (stream-cdr s))))
