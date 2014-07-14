;; representing pairs as the number (2^a)(3^b)
(define (cons a b)
  (* (expt 2 a) (expt 3 b)))

(define (car p)
  (if (= 0 (modulo p 3))
      (car (/ p 3))
      (log-base p 2)))

(define (cdr p)
  (if (= 0 (modulo p 2))
      (cdr (/ p 2))
      (log-base p 3)))


;;log with arbitrary base (is this really not built-in?!)
(define (log-base x base)
  (/ (log x) (log base)))
