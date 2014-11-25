(load "ch5-regsim.scm")

;; 2n-2 stack pushes are needed to calculate n!

(define fact-machine
  (make-machine
   '(n continue val)
   (list (list '- -) (list '* *) (list '= =))
  '(
   (assign continue (label fact-done))     ; set up final return address
   (perform (op initialize-stack))
fact-loop
   (test (op =) (reg n) (const 1))
   (branch (label base-case))
   ;; Set up for the recursive call by saving n and continue.
   ;; Set up continue so that the computation will continue
   ;; at after-fact when the subroutine returns.
   (save continue)
   (save n)
   (assign n (op -) (reg n) (const 1))
   (assign continue (label after-fact))
   (goto (label fact-loop))
 after-fact
   (restore n)
   (restore continue)
   (assign val (op *) (reg n) (reg val))   ; val now contains n(n-1)!
   (goto (reg continue))                   ; return to caller
 base-case
   (assign val (const 1))                  ; base case: 1!=1
   (goto (reg continue))                   ; return to caller
 fact-done
   (perform (op print-stack-statistics)))))

(set-register-contents! fact-machine 'n 8)
(set-register-contents! fact-machine 'val 1)
(start fact-machine)
(get-register-contents fact-machine 'val)
