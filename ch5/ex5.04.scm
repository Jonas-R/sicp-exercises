(load "ch5-regsim.scm")

(define expt-rec-machine
  (make-machine
   '(b n val continue)
   (list (list '* *) (list '= =) (list '- -))
   '( (assign continue (label expt-done))
     test-b
      (test (op =) (reg n) (const 0))
      (branch (label base-case))
      (save continue)
      (save n)
      (assign n (op -) (reg n) (const 1))
      (assign continue (label after-expt))
      (goto (label test-b))
     after-expt
      (restore n)
      (restore continue)
      (assign val (op *) (reg b) (reg val))
      (goto (reg continue))
     base-case
      (assign val (const 1))
      (goto (reg continue))
    expt-done)))

(set-register-contents! expt-rec-machine 'b 2)
(set-register-contents! expt-rec-machine 'n 9)
(start expt-rec-machine)
(display (get-register-contents expt-rec-machine 'val))
(newline)

(define expt-iter-machine
  (make-machine
   '(b n counter product)
   (list (list '* *) (list '= =) (list '- -))
   '( (assign counter (reg n))
      (assign product (const 1))
     test-b
      (test (op =) (reg counter) (const 0))
      (branch (label expt-done))
      (assign counter (op -) (reg counter) (const 1))
      (assign product (op *) (reg product) (reg b))
      (goto (label test-b))
    expt-done)))

(set-register-contents! expt-iter-machine 'b 2)
(set-register-contents! expt-iter-machine 'n 9)
(start expt-iter-machine)
(display (get-register-contents expt-iter-machine 'product))
