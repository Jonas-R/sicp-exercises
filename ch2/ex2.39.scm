(define (reverse sequence)
  (fold-right (lambda (x y) (append y (list x))) '() sequence))

(define (reverse_ sequence)
  (fold-left (lambda (x y) (append (list y) x)) '() sequence))
