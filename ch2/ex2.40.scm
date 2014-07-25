(load "flatmap.scm")
(load "enumerate.scm")
(load "accumulate.scm")
(load "prime.scm")

(define (unique-pairs n)
  (flatmap (lambda (i) (map (lambda (j) (list i j))
			    (enumerate-interval 1 (- i 1))))
	   (enumerate-interval 1 n)))

(define (make-pair-sum pair)
  (list (car pair) (cadr pair) (+ (car pair) (cadr pair))))

(define (prime-sum? pair)
  (prime? (+ (car pair) (cadr pair))))

(define (prime-sum-pairs n)
  (map make-pair-sum
       (filter prime-sum? (unique-pairs n))))
