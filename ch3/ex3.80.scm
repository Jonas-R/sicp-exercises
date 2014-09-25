(define (RLC R C L dt il0 vc0)
  (define vc (integral (delay dvc) vc0 dt))
  (define il (integral (delay dil) il0 dt))
  (define dvc (scale-stream il (/ (- 1) C)))
  (define dil (add-streams (scale-stream vc (/ 1 L)) (scale-stream il (/ (- R) L))))
  (stream-map cons vc il))

;;  (display-stream (RLC 1 0.2 1 0.1 0 10) 20)

(define (add-streams s1 s2)
  (stream-map + s1 s2))
(define (scale-stream stream factor)
  (stream-map (lambda (x) (* x factor)) stream))
