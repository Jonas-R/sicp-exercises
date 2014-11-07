;; run (initialize-data-base last-pair-database) in regular scheme to initialize

(define last-pair-database 
  '((rule (last-pair (?v) (?v)))
    (rule (last-pair (?v . ?z) ?x)
	  (last-pair ?z ?x))))

(last-pair (3) ?x)
(last-pair (1 2 3) ?x)
(last-pair (2 ?x) (3))
;; this enters an infinite loop
(last-pair ?x (3))
