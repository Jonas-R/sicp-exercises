;; merging pairs according to a weight function

(define (pairs-weighted s t weight)
  (cons-stream
   (list (stream-car s) (stream-car t))
   (merge-weighted
    (stream-map (lambda (x) (list (stream-car s) x))
                (stream-cdr t))
    (pairs-weighted (stream-cdr s) (stream-cdr t) weight)
    weight)))


(define (merge-weighted s1 s2 weight)
  (cond ((stream-null? s1) s2)
        ((stream-null? s2) s1)
        (else
         (let* ((s1car (stream-car s1))
		(s2car (stream-car s2))
		(w1 (weight s1car))
		(w2 (weight s2car)))
           (if (< w1 w2)
	       (cons-stream s1car (merge-weighted (stream-cdr s1) s2 weight))
	       (cons-stream s2car (merge-weighted s1 (stream-cdr s2) weight)))))))

(define (integers-starting-from n)
  (cons-stream n (integers-starting-from (+ n 1))))
(define integers (integers-starting-from 1))
(define weighted-int-pairs (pairs-weighted integers integers (lambda (p) (+ (car p) (cadr p)))))

;; (i j) where neither i nor j are divisible by 2, 3 or 5
;; ordered according to 2i+3j+5ij 
(define indivisible (stream-filter (lambda (pair)
				     (let ((i (car pair))
					   (j (cadr pair)))
				       (not (or (= (modulo i 2) 0) (= (modulo i 3) 0) 
						(= (modulo i 5) 0) (= (modulo j 2) 0)
						(= (modulo j 3) 0) (= (modulo j 5) 0)))))
				   (pairs-weighted integers 
						   integers
						   (lambda (pair)
						     (let ((i (car pair))
							   (j (cadr pair)))
						       (+ (* 2 i) (* 3 j) (* 5 i j)))))))
