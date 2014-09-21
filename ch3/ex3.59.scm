(define (integrate-series as)
  (define (factor-series n) 
    (cons-stream (/ 1 n) (factor-series (+ n 1))))
  (mul-streams as (factor-series 1)))

(define exp-series
  (cons-stream 1 (integrate-series exp-series)))

(define sine-series 
  (cons-stream 0
	       (integrate-series cosine-series)))

(define cosine-series
  (cons-stream 1
	       (stream-map (lambda (x) (- x)) 
			   (integrate-series sine-series))))


(define (mul-streams s1 s2) (stream-map * s1 s2))


