(load "ch5-regsim.scm")

(define count-leaves-machine
  (make-machine
   '(tree continue val tmp1)
   (list (list '+ +) (list 'null? null?) (list 'car car) (list 'cdr cdr) (list 'pair? pair?) (list 'not not))
  '(
   (assign continue (label count-done))
   (perform (op initialize-stack))
count-loop
   ;;base case null?
   (test (op null?) (reg tree))
   (branch (label base-case-null))
   ;;base case not pair?
   (assign tmp1 (op pair?) (reg tree))
   (test (op not) (reg tmp1))
   (branch (label base-case-not-pair))

   (save continue)
   (save tree)
   (assign tree (op car) (reg tree))
   (assign continue (label after-count1))
   (goto (label count-loop))
 after-count1
   (restore tree)
   (restore continue)
   
   (save val)   ; val now contains (count-leaves (car tree))
   (save continue)
   (save tree)
   (assign tree (op cdr) (reg tree))
   (assign continue (label after-count2))
   (goto (label count-loop))
 after-count2
   (restore tree)
   (restore continue)
   (assign tmp1 (reg val)) ; tmp1 now contains (count-leaves (cdr tree))
   (restore val)
   (assign val (op +) (reg tmp1) (reg val))
   (goto (reg continue)) 
 base-case-null
   (assign val (const 0))
   (goto (reg continue))
 base-case-not-pair
   (assign val (const 1))
   (goto (reg continue))
 count-done
   )))

(set-register-contents! count-leaves-machine 'tree '(1 2 3 4 (5 6 7) (8 9)))
(set-register-contents! count-leaves-machine 'val 0)
(start count-leaves-machine)
(get-register-contents count-leaves-machine 'val)
