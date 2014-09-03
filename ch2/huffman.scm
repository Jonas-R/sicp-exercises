;; huffman trees, code taken from book

(define (make-leaf symbol weight)
  (list 'leaf symbol weight))

(define (leaf? object)
  (eq? (car object) 'leaf))

(define (symbol-leaf x) (cadr x))

(define (weight-leaf x) (caddr x))

(define (make-code-tree left right)
  (list left
        right
        (append (symbols left) (symbols right))
        (+ (weight left) (weight right))))

(define (left-branch tree) (car tree))

(define (right-branch tree) (cadr tree))

(define (symbols tree)
  (if (leaf? tree)
      (list (symbol-leaf tree))
      (caddr tree)))

(define (weight tree)
  (if (leaf? tree)
      (weight-leaf tree)
      (cadddr tree)))

;; decoding
(define (decode bits tree)
  (define (decode-1 bits current-branch)
    (if (null? bits)
        '()
        (let ((next-branch
               (choose-branch (car bits) current-branch)))
          (if (leaf? next-branch)
              (cons (symbol-leaf next-branch)
                    (decode-1 (cdr bits) tree))
              (decode-1 (cdr bits) next-branch)))))
  (decode-1 bits tree))

(define (choose-branch bit branch)
  (cond ((= bit 0) (left-branch branch))
        ((= bit 1) (right-branch branch))
        (else (error "bad bit -- CHOOSE-BRANCH" bit))))

;; encoding (exercise 2.68)
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

;; generate huffman tree (exercise 2.69)
(define (generate-huffman-tree pairs)
  (successive-merge (make-leaf-set pairs)))

(define (successive-merge pairset)
  (if (= (length pairset) 1) 
      (car pairset)
      (let ((new-node (make-code-tree (car pairset) (cadr pairset))))
	   (successive-merge (adjoin-set new-node (cddr pairset))))))

(define (adjoin-set x set)
  (if (null? set)
      (list x)
      (cond ((>= (weight x) (weight (car set))) (cons (car set) (adjoin-set x (cdr set))))
	    (else (cons x set)))))

(define (make-leaf-set pairs)
  (if (null? pairs)
      '()
      (let ((pair (car pairs)))
        (adjoin-set (make-leaf (car pair)    ; symbol
                               (cadr pair))  ; frequency
                    (make-leaf-set (cdr pairs))))))

