;; load partial-sums procedure
(load "ex3.55.scm")
(load "display_stream.scm")

;; approximate ln(2)

(define (ln-summands n)
  (cons-stream (/ 1.0 n)
	       (stream-map - (ln-summands (+ n 1)))))

(define ln-stream
  (partial-sums (ln-summands 1)))

(display-line "Regular approximation:")
(display-stream ln-stream 10)

(define (euler-transform s)
  (let ((s0 (stream-ref s 0))
        (s1 (stream-ref s 1))    
        (s2 (stream-ref s 2)))
    (cons-stream (- s2 (/ (square (- s2 s1))
                          (+ s0 (* -2 s1) s2)))
                 (euler-transform (stream-cdr s)))))

(display-line "Approximation using Euler transform")
(display-stream (euler-transform ln-stream) 10)

(define (make-tableau transform s)
  (cons-stream s
               (make-tableau transform
                             (transform s))))

(define (accelerated-sequence transform s)
  (stream-map stream-car
              (make-tableau transform s)))

(display-line "Approximation using recursive Euler transform")
(display-stream (accelerated-sequence euler-transform
				      ln-stream) 10)
