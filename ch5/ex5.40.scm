(load "ex5.39.scm")

(define (compile exp target linkage comp-env)
  (cond ((self-evaluating? exp)
         (compile-self-evaluating exp target linkage comp-env))
        ((quoted? exp) (compile-quoted exp target linkage comp-env))
        ((variable? exp)
         (compile-variable exp target linkage comp-env))
        ((assignment? exp)
         (compile-assignment exp target linkage comp-env))
        ((definition? exp)
         (compile-definition exp target linkage comp-env))
        ((if? exp) (compile-if exp target linkage))
        ((lambda? exp) (compile-lambda exp target linkage comp-env))
        ((begin? exp)
         (compile-sequence (begin-actions exp)
                           target
                           linkage
			   comp-env))
        ((cond? exp) (compile (cond->if exp) target linkage comp-env))
        ((application? exp)
         (compile-application exp target linkage comp-env))
        (else
         (error "Unknown expression type -- COMPILE" exp))))

(define (compile-lambda exp target linkage comp-env)
  (let ((proc-entry (make-label 'entry))
        (after-lambda (make-label 'after-lambda)))
    (let ((lambda-linkage
           (if (eq? linkage 'next) after-lambda linkage)))
      (append-instruction-sequences
       (tack-on-instruction-sequence
        (end-with-linkage lambda-linkage
         (make-instruction-sequence '(env) (list target)
          `((assign ,target
                    (op make-compiled-procedure)
                    (label ,proc-entry)
                    (reg env)))))
        (compile-lambda-body exp proc-entry comp-env))
       after-lambda))))

(define (compile-lambda-body exp proc-entry comp-env)
  (let ((formals (lambda-parameters exp)))
    (append-instruction-sequences
     (make-instruction-sequence '(env proc argl) '(env)
      `(,proc-entry
        (assign env (op compiled-procedure-env) (reg proc))
        (assign env
                (op extend-environment)
                (const ,formals)
                (reg argl)
                (reg env))))
     (compile-sequence (lambda-body exp) 'val 'return (cons formals comp-env)))))

(define (extend-compilation-environment comp-env new-variables)
  (let ((vals (map (lambda (x) '*unassigned*) new-variables)))
    (extend-environment new-variables vals env)))

(define (compile-linkage linkage)
  (cond ((eq? linkage 'return)
         (make-instruction-sequence '(continue) '()
          '((goto (reg continue)))))
        ((eq? linkage 'next)
         (empty-instruction-sequence))
        (else
         (make-instruction-sequence '() '()
          `((goto (label ,linkage)))))))

(define (compile-self-evaluating exp target linkage comp-env)
  (end-with-linkage linkage
   (make-instruction-sequence '() (list target)
    `((assign ,target (const ,exp))))))

(define (compile-quoted exp target linkage comp-env)
  (end-with-linkage linkage
   (make-instruction-sequence '() (list target)
    `((assign ,target (const ,(text-of-quotation exp)))))))

(define (compile-variable exp target linkage comp-env)
  (end-with-linkage linkage
   (make-instruction-sequence '(env) (list target)
    `((assign ,target
              (op lookup-variable-value)
              (const ,exp)
              (reg env))))))

(define (compile-assignment exp target linkage comp-env)
  (let ((var (assignment-variable exp))
        (get-value-code
         (compile (assignment-value exp) 'val 'next)))
    (end-with-linkage linkage
     (preserving '(env)
      get-value-code
      (make-instruction-sequence '(env val) (list target)
       `((perform (op set-variable-value!)
                  (const ,var)
                  (reg val)
                  (reg env))
         (assign ,target (const ok))))))))

(define (compile-definition exp target linkage comp-env)
  (let ((var (definition-variable exp))
        (get-value-code
         (compile (definition-value exp) 'val 'next comp-env)))
    (end-with-linkage linkage
     (preserving '(env)
      get-value-code
      (make-instruction-sequence '(env val) (list target)
       `((perform (op define-variable!)
                  (const ,var)
                  (reg val)
                  (reg env))
         (assign ,target (const ok))))))))

(define (compile-if exp target linkage comp-env)
  (let ((t-branch (make-label 'true-branch))
        (f-branch (make-label 'false-branch))                    
        (after-if (make-label 'after-if)))
    (let ((consequent-linkage
           (if (eq? linkage 'next) after-if linkage)))
      (let ((p-code (compile (if-predicate exp) 'val 'next comp-env))
            (c-code
             (compile
              (if-consequent exp) target consequent-linkage comp-env))
            (a-code
             (compile (if-alternative exp) target linkage comp-env)))
        (preserving '(env continue)
         p-code
         (append-instruction-sequences
          (make-instruction-sequence '(val) '()
           `((test (op false?) (reg val))
             (branch (label ,f-branch))))
          (parallel-instruction-sequences
           (append-instruction-sequences t-branch c-code)
           (append-instruction-sequences f-branch a-code))
          after-if))))))

(define (compile-sequence seq target linkage comp-env)
  (if (last-exp? seq)
      (compile (first-exp seq) target linkage comp-env)
      (preserving '(env continue)
       (compile (first-exp seq) target 'next comp-env)
       (compile-sequence (rest-exps seq) target linkage comp-env))))

(define (compile-application exp target linkage comp-env)
  (let ((proc-code (compile (operator exp) 'proc 'next comp-env))
        (operand-codes
         (map (lambda (operand) (compile operand 'val 'next comp-env))
              (operands exp))))
    (preserving '(env continue)
     proc-code
     (preserving '(proc continue)
      (construct-arglist operand-codes)
      (compile-procedure-call target linkage comp-env)))))
