;; load heap implementation
(load "../ch2/ex2.66.scm")

;; table structure using binary heap

(define (make-table comp-op)
  (let ((local-table '()))
    (define (lookup key)
      (heap-lookup key local-table comp-op))
    (define (insert! key value)
      (set! local-table (adjoin-set (make-record key value) local-table))
      'ok)      
    (define (dispatch m)
      (cond ((eq? m 'lookup-proc) lookup)
            ((eq? m 'insert-proc!) insert!)
            (else (error "Unknown operation -- TABLE" m))))
    dispatch))

(define operation-table (make-table <))
(define get (operation-table 'lookup-proc))
(define put (operation-table 'insert-proc!))

;; modified to work with any comparison operator
(define (heap-lookup given-key set-of-records comp-op)
  (if (null? set-of-records) 
      #f
      (let ((key (get-key (entry set-of-records))))
	(cond ((comp-op given-key key) (heap-lookup given-key (left-branch set-of-records) comp-op))
	      ((comp-op key given-key) (heap-lookup given-key (right-branch set-of-records) comp-op))
	      (else (get-data (entry set-of-records)))))))
