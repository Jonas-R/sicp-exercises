;; solve general second-order linear differential equations

(define (solve-2nd-general f dt y0 dy0)
  (define y (integral (delay dy) y0 dt))
  (define dy (integral (delay ddy) dy0 dt))
  (define ddy (f dy y))
  y)

(define (add-streams s1 s2)
  (stream-map + s1 s2))
(define (scale-stream stream factor)
  (stream-map (lambda (x) (* x factor)) stream))

#|
USAGE EXAMPLE:

solves y'' - 2y' - 4y = 0
(solve-2nd-general (lambda (dy y) (add-streams (scale-stream dy 2) (scale-stream y 4))) 0.001 1 1)
|#
