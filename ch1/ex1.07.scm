;; An implementation of sqrt that keeps track of the previous guess
;; and returns when the change is less than 0.01% of the guess.

(define (sqrt x)
  (sqrt-iter 1.0 0.9 x))

(define (sqrt-iter guess old-guess x)
  (if (good-enough? guess old-guess x)
      guess
      (sqrt-iter (improve guess x)
		 guess
		 x)))

(define (improve guess x)
  (average guess (/ x guess)))

(define (average x y)
  (/ (+ x y) 2))

(define (good-enough? guess old-guess x)
  (< (/ (abs (- guess old-guess)) guess) 0.0001))
