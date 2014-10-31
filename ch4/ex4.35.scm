;; load into ambiguous evaluator to use

(define (an-integer-between i j)
  (require (<= i j))
  (amb i (an-integer-between (+ i 1) j)))

(define (require p)
  (if (not p) (amb)))

(define (a-pythagorean-triple-between low high)
  (let ((i (an-integer-between low high)))
    (let ((j (an-integer-between i high)))
      (let ((k (an-integer-between j high)))
        (require (= (+ (* i i) (* j j)) (* k k)))
        (list i j k)))))
