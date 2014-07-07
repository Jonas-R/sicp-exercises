;; probabilistically searches for primes in a range and displaying running time, uses little fermat to achieve O(log n) running time

(define (search-for-primes start end)
  (cond ((> start end) ) 
	((even? start) (search-for-primes (+ start 1) end))
	(else (timed-prime-test start)
	      (search-for-primes (+ start 2) end))))

(define (timed-prime-test n)
  (newline)
  (display n)
  (start-prime-test n (runtime)))

(define (start-prime-test n start-time)
  (if (prime? n 1000)
      (report-prime (- (runtime) start-time))))

(define (report-prime elapsed-time)
  (display " *** ")
  (display elapsed-time))

;;primality testing in O(log(n))
(define (prime? n times)
  (cond ((= times 0) true)
	((fermat-test n) (prime? n (- times 1)))
	(else false)))

(define (fermat-test n)
  (define (try-it a)
    (= (expmod a n n) a))
  (try-it (+ 1 (random (- n 1)))))

(define (expmod base exp m)
  (cond ((= exp 0) 1)
	((even? exp)
	 (remainder (square (expmod base (/ exp 2) m))
		    m))
	(else
	 (remainder (* base (expmod base (- exp 1) m))
		    m))))
