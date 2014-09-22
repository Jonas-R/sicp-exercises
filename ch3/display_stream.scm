(define (display-stream s n)
  (stream-for-each-n n display-line s))

(define (display-line x)
  (newline)
  (display x))

(define (stream-for-each-n n proc s)
  (if (or (stream-null? s) (= n 0))
      'done
      (begin (proc (stream-car s))
	     (stream-for-each-n (- n 1) proc (stream-cdr s)))))
