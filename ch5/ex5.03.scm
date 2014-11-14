(load "ch5-regsim.scm")

;; register state machine for square root approximation using Newton's method
(define newton-machine
  (make-machine
   '(guess x tmp1)
   (list (list '* *) (list '+ +) (list '< <) (list 'abs abs) (list '- -) (list '/ /))
   '(test-b
      ;; good-enough?
      (assign tmp1 (op *) (reg guess) (reg guess))
      (assign tmp1 (op -) (reg tmp1) (reg x))
      (assign tmp1 (op abs) (reg tmp1))
      (test (op <) (reg tmp1) (const 0.001))
      (branch (label newton-done))
      ;; improve
      (assign tmp1 (op /) (reg x) (reg guess))
      (assign tmp1 (op +) (reg guess) (reg tmp1))
      (assign guess (op /) (reg tmp1) (const 2.0))
      (goto (label test-b))
    newton-done)))

(set-register-contents! newton-machine 'guess 1.0)
(set-register-contents! newton-machine 'x 81)
(start newton-machine)
(get-register-contents newton-machine 'guess)
