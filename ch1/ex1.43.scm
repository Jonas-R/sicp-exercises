;;import function composition
(load "ex1.42.scm")

;;repeated application of a function
(define (repeated f n)
  (if (= n 1)
      f
      (repeated (compose f f) (- n 1))))
