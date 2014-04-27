;;linear time, constant space integer multiplication using addition, double and halve

(define (fast-mult n m)
  (cond ((= m 0) 0)
	((even? m) (double (fast-mult n (halve m))))
	(else (+ n (fast-mult n (- m 1))))))


(define (double n) (+ n n))
(define (halve n) (/ n 2))

(define (even? n) (= 0 (remainder n 2)))
