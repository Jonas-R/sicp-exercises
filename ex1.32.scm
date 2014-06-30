;; accumulate (similar to fold)
(define (accumulate combiner null-value term a next b)
  (if (> a b)
      null-value
      (combiner (term a)
		(accumulate combiner null-value term (next a) next b))))

(define (accumulate-iter combiner null-value term a next b)
  (define (iter a acc)
    (if (> a b)
	acc
	(iter (next a) (combiner acc (term a)))))
  (iter a null-value))
