;; iterative implementation of sum
(define (sum term a next b)
  (define (iter a result)
    (if (> a b)
	result
	(iter (+ a 1) (+ result (term a)))))
  (iter 0 0))
