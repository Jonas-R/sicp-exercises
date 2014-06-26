;; Computes an approximate integral using simpson's rule
(define (simpson-integral f a b n)
  (define (get-h) (/ (- b a) n))
  (define (get-multiplier k)
      (if (= 0 (modulo k 2)) 2 4))
  (define (simpson-helper f a b k acc)
      (cond ((= k n) (/ (* (get-h) (+ acc (f (+ a (* k (get-h)))))) 3))
	    ((= k 0) (simpson-helper f a b (+ k 1) (+ acc (f a))))
	    (else (simpson-helper f a b (+ k 1) (+ acc (* (get-multiplier k) (f (+ a (* k (get-h))))))))))
  (simpson-helper f a b 0 0))
  
