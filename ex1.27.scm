;;tests wether a number is carmichael (or prime)

(define (carmichael? n)
  (define (carmichael-helper n a)
    (cond ((= n a) #t)
	  ((= a (expmod a n n)) (carmichael-helper n (+ a 1)))
	  (else #f)))
  (carmichael-helper n 1))

(define (expmod base exp m)
  (cond ((= exp 0) 1)
	((even? exp)
	 (remainder (square (expmod base (/ exp 2) m))
		    m))
	(else
	 (remainder (* base (expmod base (- exp 1) m))
		    m))))
