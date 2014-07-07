;;load prime? function
(load "ex1.28.scm")

(define (filtered-accumulate combiner filter null-value term a next b)
  (if (> a b)
      null-value
      (if (filter a)
	  (combiner (term a)
		    (filtered-accumulate combiner filter null-value term (next a) next b))
	  (filtered-accumulate combiner filter null-value term (next a) next b))))

;;example a: calculate the sum of squares of primes in [a,b]
(define (sum-of-squares-of-primes a b)
  (define (inc x) (+ x 1))
  (filtered-accumulate + miller-rabin-test 0 square a inc b))

;;example b: calculate the product of positive integers <n that are relatively prime to n
(define (product-of-relatively-prime n)
  (define (inc x) (+ x 1))
  (define (identity x) x)
  (define (relatively-prime x)
    (= (gcd x n) 1))
  (filtered-accumulate * relatively-prime 1 identity 0 inc n))
