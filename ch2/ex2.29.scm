;;binary mobile

(define (make-mobile left right)
  (list left right))

(define (make-branch length structure)
  (list length structure))

(define (left-branch  mobile) (car mobile))
(define (right-branch mobile) (cadr mobile))

(define (branch-length branch) (car branch))
(define (branch-structure branch) (cadr branch))

(define (weight-of-branch branch)
  (if (pair? (branch-structure branch))
      (total-weight (branch-structure branch))
      (branch-structure branch)))
      

(define (total-weight mobile)
  (+ (weight-of-branch (left-branch mobile)) (weight-of-branch (right-branch mobile))))

(define (balanced? mobile)
  (cond
   ;; reached leaf
   ((not (pair? mobile)) #t)
   ;; mobile is balanced
   ((= (* (branch-length (left-branch mobile)) (weight-of-branch (left-branch mobile)))
       (* (branch-length (right-branch mobile)) (weight-of-branch (right-branch mobile))))
    (and (balanced? (branch-structure (left-branch mobile))) 
	 (balanced? (branch-structure (right-branch mobile)))))
   ;; unbalanced
   (#t #f)))

;;ex d: only the accessors would need to be changed, as the other functions build on them exclusively:
