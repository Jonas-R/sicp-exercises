;; probabilistically searches for primes in a range and displaying running time, uses miller-rabin test to achieve O(log n) running time

(define (search-for-primes start end)
  (cond ((> start end) ) 
	((even? start) (search-for-primes (+ start 1) end))
	(else (timed-prime-test start)
	      (search-for-primes (+ start 2) end))))

(define (timed-prime-test n)
  (newline)
  (display n)
  (start-prime-test n (runtime)))

; the second parameter to prime? 1000 should really not be constant
(define (start-prime-test n start-time)
  (if (prime? n 1000)
      (report-prime (- (runtime) start-time))))

(define (report-prime elapsed-time)
  (display " *** ")
  (display elapsed-time))

;;primality testing in O(log(n))
(define (prime? n times)
  (cond ((= times 0) true)
	((miller-rabin-test n) (prime? n (- times 1)))
	(else false)))

(define (miller-rabin-test n)
  (define (try-it a)
    (= (expmod-miller-rabin a (- n 1) n) 1))
  (try-it (+ 1 (random (- n 1)))))

(define (expmod-miller-rabin base exp m)
  (cond ((= exp 0) 1)
	((even? exp)
	 (let* ((cur-num (expmod-miller-rabin base (/ exp 2) m))
	       (root (remainder (square cur-num) m)))
	 (cond ((and (not (= cur-num 1)) (not (= cur-num (- m 1))) (= root 1)) 0)
	       (else root))))
	(else
	 (remainder (* base (expmod-miller-rabin base (- exp 1) m))
		    m))))
