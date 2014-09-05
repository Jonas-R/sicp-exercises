;; Insatiable Enterprises Inc. personnel records

(define (get-record employee file)
  ((get 'record (get-division file)) (get-file file) employee))

(define (get-salary employee file)
  ((get 'salary (get-division file)) (get-file file) employee))

(define (find-employee-record employee files)
  (if (null? files)
      #f
      (let ((record (get-record employee (car files))))
	(if record 
	    record 
	    (find-employee-record employee (cdr files))))))

(define (make-typed-file division file) (cons division file))
(define (get-division file) (car file))
(define (get-file file) (cdr file))

;;example representations
(define (make-record-minimal employee salary) (cons employee salary))
(define (get-employee-minimal record) (car record))
(define (get-salary-minimal record) (cdr record))


(define (install-minimal-package)
  ;;internal procedures
  (define (get-record file employee) 
    (if (null? file) 
	#f
	(let ((cur-emp (get-employee-minimal (car file))))
	  (if (equal? employee cur-emp)
	      (car file)
	      (get-record (cdr file) employee)))))
  (define (get-salary file employee) 
    (get-salary-minimal (get-record file employee)))

  ;;interface to rest of system
  (put 'record 'minimal get-record)
  (put 'salary 'minimal get-salary)
  'done)

;; dispatch table implementation
(define dispatch-table (make-equal-hash-table))

(define (put op type-tag function)
  (hash-table/put! dispatch-table (list op type-tag) function))

(define (get op type-tag)
  (hash-table/get dispatch-table (list op type-tag) '()))

(install-minimal-package)

#|
USAGE EXAMPLE:
(define rec1 (make-record-minimal "Dave" 10000))
(define rec2 (make-record-minimal "Amanda" 15000))
(define file (make-typed-file 'minimal (list rec1 rec2)))
(get-record "Dave" file)
(get-salary "Amanda" file)
(find-employee-record "Dave" (list file))

|#
