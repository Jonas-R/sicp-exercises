;; streams of triplets

(define (interleave s1 s2)
  (if (stream-null? s1)
      s2
      (cons-stream (stream-car s1)
                   (interleave s2 (stream-cdr s1)))))

;; triplets (s_i t_j u_k) where i <= j <= k
(define (triplets s t u)
  (let ((t-u-pairs (pairs t u)))
    (cons-stream (append (list (stream-car s))
			 (stream-car t-u-pairs))
		 (interleave 
		  (stream-map (lambda (x) (append (list (stream-car s)) x))
			      (stream-cdr t-u-pairs))
		  (triplets (stream-cdr s) (stream-cdr t) (stream-cdr u))))))

(define (pairs s t)
  (cons-stream
   (list (stream-car s) (stream-car t))
   (interleave
    (stream-map (lambda (x) (list (stream-car s) x))
                (stream-cdr t))
    (pairs (stream-cdr s) (stream-cdr t)))))

(define (integers-starting-from n)
  (cons-stream n (integers-starting-from (+ n 1))))

(define integers (integers-starting-from 1))

(define int-triplets (triplets integers integers integers))

(define pythagorean-triplets (stream-filter (lambda (triplet)
					      (let ((i (car triplet))
						    (j (cadr triplet))
						    (k (caddr triplet)))
						(= (+ (expt i 2) (expt j 2)) (expt k 2))))
					    int-triplets))

