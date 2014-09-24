;; simulating an RC circuit using streams
(define (RC R C dt)
  (define (voltage-stream c-stream v0)
    (add-streams (integral (scale-stream c-stream (/ 1 C)) v0 dt) 
		 (scale-stream c-stream R)))
  voltage-stream)
		  
(define (integral integrand initial-value dt)
  (define int
    (cons-stream initial-value
                 (add-streams (scale-stream integrand dt)
                              int)))
  int)

(define (scale-stream stream factor)
  (stream-map (lambda (x) (* x factor)) stream))
(define (add-streams s1 s2)
  (stream-map + s1 s2))
