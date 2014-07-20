(define (reverse l)
  (if (null? l)
      '()
      (append (reverse (cdr l)) (list (car l))))) 
