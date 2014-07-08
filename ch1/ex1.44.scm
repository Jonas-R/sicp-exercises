(define dx 0.00001)
;;smoothing a function
(define (smooth f)
  (lambda (x) (/ (+ (f x) (f (+ x dx)) (f (- x dx))) 3)))

;;n-fold smoothing
(load "ex1.43.scm")
(define (n-fold-smooth f n)
  ((repeated smooth n) f))
