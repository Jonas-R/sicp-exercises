(define (mul-streams s1 s2)
  (stream-map * s1 s2))

(define (integers-starting-from n) (cons-stream n (integers-starting-from (+ n 1))))
(define factorials (cons-stream 1 (mul-streams factorials (integers-starting-from 2))))
