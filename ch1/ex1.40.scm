(load "newtons_method.scm")
;; cubic function with coefficients a,b,c
(define (cubic a b c)
  (lambda (x) (+ (expt x 3) (* a (expt x 2)) (* b x) c)))

;; approximates zeros of the cubic function with coefficients a,b,c
(define (cubic-zeros a b c)
  (newtons-method (cubic a b c) 1))

