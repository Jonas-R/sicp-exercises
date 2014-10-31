;; load into ambiguous evaluator to use

(define (an-integer-starting-from i)
  (amb i (an-integer-starting-from (+ i 1))))

(define (require p)
  (if (not p) (amb)))

(define (a-pythagorean-triple)
  (define (next-triplet i j k)
    (cond ((= i j k) (list 1 1 (+ k 1)))
	  ((= i j) (list 1 (+ j 1) k))
	  (else (list (+ i 1) j k))))
  (define (a-triplet start)
    (amb start  (a-triplet (next-triplet (car start)
					 (car (cdr start))
					 (car (cdr (cdr start)))))))
  (let ((triplet (a-triplet (list 1 1 1))))
    (let ((i (car triplet))
	  (j (car (cdr triplet)))
	  (k (car (cdr (cdr triplet)))))
      (require (= (+ (* i i) (* j j)) (* k k)))
      triplet)))
