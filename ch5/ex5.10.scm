(load "ch5-regsim.scm")

;; some very arbitrary syntax changes

(define (register-exp? exp) (tagged-list? exp 'reg))

(define (register-exp-reg exp) (cadr exp))

(define (constant-exp? exp) (tagged-list? exp 'kon))

(define (constant-exp-value exp) (cadr exp))

(define (label-exp? exp) (tagged-list? exp 'mk))

(define (label-exp-label exp) (cadr exp))


(define expt-rec-machine
  (make-machine
   '(b n val continue)
   (list (list '* *) (list '= =) (list '- -))
   '( (assign continue (mk expt-done))
     test-b
      (test (op =) (reg n) (kon 0))
      (branch (mk base-case))
      (save continue)
      (save n)
      (assign n (op -) (reg n) (kon 1))
      (assign continue (mk after-expt))
      (goto (mk test-b))
     after-expt
      (restore n)
      (restore continue)
      (assign val (op *) (reg b) (reg val))
      (goto (reg continue))
     base-case
      (assign val (kon 1))
      (goto (reg continue))
    expt-done)))
