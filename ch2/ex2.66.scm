;; lookup for sets of records represented by binary heaps
(define (heap-lookup given-key set-of-records)
  (if (null? set-of-records) 
      #f
      (let ((key (get-key (entry set-of-records))))
	(cond ((= given-key key) (get-data (entry set-of-records)))
	      ((< given-key key) (heap-lookup given-key (left-branch set-of-records)))
	      ((> given-key key) (heap-lookup given-key (right-branch set-of-records)))))))

(define (make-record key data) (cons key data))
(define (get-key record) (car record))
(define (get-data record) (cdr record)) 


#|
USAGE EXAMPLE:

(define rec1 (make-record 1 "Larry Page"))
(define rec2 (make-record 2 "Sergey Brin"))
(define rec3 (make-record 8 "Eric Schmidt"))
(define rec4 (make-record 25 "Marissa Meyer"))
(define rec5 (make-record #xDEADBEEF "Jonas Raedle"))

(define record-tree (adjoin-set rec3 '()))
(define record-tree (adjoin-set rec5 record-tree))
(define record-tree (adjoin-set rec4 record-tree))
(define record-tree (adjoin-set rec1 record-tree))
(define record-tree (adjoin-set rec2 record-tree))

(heap-lookup 8 record-tree)
(heap-lookup 10 record-tree)

|#

(define (entry tree) (car tree))

(define (left-branch tree) (cadr tree))

(define (right-branch tree) (caddr tree))

(define (make-tree entry left right)
  (list entry left right))

(define (element-of-set? x set)
  (cond ((null? set) false)
        ((= (get-key x) (get-key (entry set))) true)
        ((< (get-key x) (get-key (entry set)))
         (element-of-set? x (left-branch set)))
        ((> (get-key x) (get-key (entry set)))
         (element-of-set? x (right-branch set)))))

(define (adjoin-set x set)
  (cond ((null? set) (make-tree x '() '()))
        ((= (get-key x) (get-key (entry set))) set)
        ((< (get-key x) (get-key (entry set)))
         (make-tree (entry set) 
                    (adjoin-set x (left-branch set))
                    (right-branch set)))
        ((> (get-key x) (get-key (entry set)))
         (make-tree (entry set)
                    (left-branch set)
                    (adjoin-set x (right-branch set))))))
