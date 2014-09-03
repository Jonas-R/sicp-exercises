(load "huffman.scm")

(define (encode message tree)
  (if (null? message)
      '()
      (append (encode-symbol (car message) tree)
	      (encode (cdr message) tree))))

(define (encode-symbol symbol tree)
  (cond ((null? tree) (error "Symbol not in tree!"))
    	((leaf? tree) '())
	((memq symbol (symbols (right-branch tree))) (cons 1 (encode-symbol symbol (right-branch tree))))
	((memq symbol (symbols (left-branch tree)))  (cons 0 (encode-symbol symbol (left-branch tree))))
	(else (error "Symbol not in tree!"))))



