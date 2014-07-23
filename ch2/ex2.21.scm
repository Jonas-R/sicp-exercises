(define (square-list items)
  (if (null? items)
      '()
      (cons (expt (car items) 2) (square-list (cdr items)))))

(define (square-list2 items)
  (map (lambda (x) (* x x)) items))
