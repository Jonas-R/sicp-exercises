;; change cond syntax to switch

(define (cond? exp) (tagged-list? exp 'switch))
