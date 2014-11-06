(define (ramb? exp) (tagged-list? exp 'ramb))
(define (ramb-choices exp) (cdr exp))

(define (analyze exp)
  (cond ((self-evaluating? exp) 
         (analyze-self-evaluating exp))
        ((quoted? exp) (analyze-quoted exp))
        ((variable? exp) (analyze-variable exp))
        ((assignment? exp) (analyze-assignment exp))
        ((definition? exp) (analyze-definition exp))
        ((if? exp) (analyze-if exp))
        ((lambda? exp) (analyze-lambda exp))
        ((begin? exp) (analyze-sequence (begin-actions exp)))
        ((cond? exp) (analyze (cond->if exp)))
        ((let? exp) (analyze (let->combination exp))) ;**
        ((ramb? exp) (analyze-ramb exp))                ;**
        ((application? exp) (analyze-application exp))
        (else
         (error "Unknown expression type -- ANALYZE" exp))))

(define (rambeval exp env succeed fail)
  ((analyze exp) env succeed fail))

(define (analyze-ramb exp)
  (let ((cprocs (shuffle (map analyze (ramb-choices exp))))) ;; randomize procedures
    (lambda (env succeed fail)
      (define (try-next choices)
        (if (null? choices)
            (fail)
            ((car choices) env
                           succeed
                           (lambda ()
                             (try-next (cdr choices))))))
      (try-next cprocs))))

;; shuffles a list, taken from list.racket-lang mailing list
(define (shuffle xs)
   (if (or (null? xs) (null? (cdr xs))) xs
       (let split ((xs xs) (odd '()) (even '()))
         (if (pair? xs)
             (split (cdr xs) (cons (car xs) even) odd)
             (let merge ((odd (shuffle odd)) (even (shuffle even)))
               (cond ((null? odd) even)
                     ((null? even) odd)
                     ((zero? (random 2)) (cons (car odd) (merge (cdr odd) even)))
                     (else (cons (car even) (merge odd (cdr even))))))))))
