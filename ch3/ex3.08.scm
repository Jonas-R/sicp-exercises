(define f
  (let ((state 1))
    (lambda (n)
      (define prev state)
      (set! state n)
      (* prev n))))

(+ (f 0) (f 1))
;; => 1

(+ (f 1) (f 0))
;; => 0
