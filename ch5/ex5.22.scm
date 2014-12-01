(load "ch5-regsim.scm")

#|
(define (append x y)
  (if (null? x)
      y
      (cons (car x) (append (cdr x) y))))

(define (append! x y)
  (set-cdr! (last-pair x) y)
  x)
|#

(define append-machine
  (make-machine
   '(x y res continue tmp1)
   (list (list 'null? null?) (list 'car car) (list 'cdr cdr) (list 'cons cons))
  '(
   (assign continue (label append-done))
append-loop
   ;;base case null?
   (test (op null?) (reg x))
   (branch (label base-case))

   (save continue)
   (save x)
   (assign x (op cdr) (reg x))
   (assign continue (label after-append))
   (goto (label append-loop))
 after-append
   (restore x)
   (restore continue)
   
   (assign tmp1 (op car) (reg x))
   (assign res (op cons) (reg tmp1) (reg res))
   (goto (reg continue))
 base-case
   (assign res (reg y))
   (goto (reg continue))
 append-done
   )))

(set-register-contents! append-machine 'x '(1 2 3 4))
(set-register-contents! append-machine 'y '(5 6 7 8))
(set-register-contents! append-machine 'res '())
(start append-machine)
(display (get-register-contents append-machine 'x))
(newline)
(display (get-register-contents append-machine 'y))
(newline)
(display (get-register-contents append-machine 'res))


(define append!-machine
  (make-machine
   '(x y continue tmp1)
   (list (list 'null? null?) (list 'car car) (list 'cdr cdr) (list 'set-cdr! set-cdr!))
  '(
   (assign continue (label append!-done))
   (assign tmp1 (reg x))
append!-loop
   ;;base case null?
   (test (op null?) (reg tmp1))
   (branch (label append!-done))

   (save continue)
   (save tmp1)
   (assign tmp1 (op cdr) (reg tmp1))
   (goto (label append!-loop))

 append!-done
   (restore tmp1)
   (perform (op set-cdr!) (reg tmp1) (reg y))
   )))

(set-register-contents! append!-machine 'x '(1 2 3 4))
(set-register-contents! append!-machine 'y '(5 6 7 8))
(start append!-machine)
(newline)
(display (get-register-contents append!-machine 'x))
(newline)
(display (get-register-contents append!-machine 'y))
