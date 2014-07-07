;; recursive and iterative implementations of the function
;; f(n) = n if n < 3; f(n) = f(n-1) + 2f(n-2) + 3f(n-3) if n >= 3

(define (recursive n)
  (cond ((< n 3) n)
	(else (+ (recursive (- n 1)) (* 2 (recursive (- n 2))) (* 3 (recursive (- n 3))))))) 

(define (iterative n)
  (define (iterative-iter a b c n)
    (cond ((= n 0) c)
	  (else (iterative-iter (+ a (* 2 b) (* 3 c)) a b (- n 1)))))
  (iterative-iter 2 1 0 n) 
)
