;; continued fractions
(define (cont-frac n d k)
  (define (cont-frac-helper i)
    (if (= i k)
      0
      (/ (n i) (+ (d i) (cont-frac-helper (+ i 1))))))
  (cont-frac-helper 1))

(define (cont-frac-iter n d k)
  (define (cont-frac-helper i acc)
    (if (= i 0)
	acc
	((cont-frac-helper (- i 1) (/ (n i) (+ (d i) acc)))))))
