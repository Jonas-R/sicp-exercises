(define (iterative-improve good-enough? improve-guess)
  (lambda (guess) (if (good-enough? guess)
		      guess
		      ((iterative-improve good-enough? improve-guess) (improve-guess guess)))))

;;sqrt in terms of iterative-improve
(define (sqrt x)
  (define (average a b) (/ (+ a b) 2))
  ((iterative-improve (lambda (guess) (< (abs (- (square guess) x)) 0.00001))
		      (lambda (guess) (average guess (/ x guess))))
   1.0))

;;fixed point in terms of iterative-improve
(define (fixed-point f first-guess)
  ((iterative-improve (lambda (guess) (< (abs (- guess (f guess))) 0.00001))
		      (lambda (guess) (f guess)))
   first-guess))
