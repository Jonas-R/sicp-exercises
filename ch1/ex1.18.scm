;;linear time, constant space integer multiplication using addition, double and halve

(define (fast-mult n m)
  (define (fast-mult-iter a n m)
    (cond ((= m 0) a)
	  ((even? m) (fast-mult-iter a (double n) (halve m)))
	  (else (fast-mult-iter (+ a n) n (- m 1)))))
  (fast-mult-iter 0 n m))


(define (double n) (+ n n))
(define (halve n) (/ n 2))

(define (even? n) (= 0 (remainder n 2)))
