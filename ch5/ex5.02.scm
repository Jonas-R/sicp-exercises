(load "ch5-regsim.scm")

;; register state machine for factorial
(define fact-machine
  (make-machine
   '(c prod n)
   (list (list '* *) (list '+ +) (list '> >))
   '(test-b
      (test (op >) (reg c) (reg n))
      (branch (label fact-done))
      (assign prod (op *) (reg prod) (reg c))
      (assign c (op +) (reg c) (const 1))
      (goto (label test-b))
    fact-done)))

(set-register-contents! fact-machine 'c 1)
(set-register-contents! fact-machine 'prod 1)
(set-register-contents! fact-machine 'n 7)
(start fact-machine)
(get-register-contents fact-machine 'prod)
