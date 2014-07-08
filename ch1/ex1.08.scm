;; Computing the cube root using newtons method

(define (cbrt x)
  (sqrt-iter 1.0 0.9 x))

(define (cbrt-iter guess old-guess x)
  (if (good-enough? guess old-guess x)
      guess
      (cbrt-iter (improve guess x)
		 guess
		 x)))

(define (improve guess x)
  (/ (+ (/ x (expt guess 2)) 
	(* 2 guess)) 
     3))

(define (average x y)
  (/ (+ x y) 2))

(define (good-enough? guess old-guess x)
  (< (/ (abs (- guess old-guess)) guess) 0.0001))
