(load "queue.scm")

(define (print-queue q)
  (display (front-ptr q))
  (newline))
