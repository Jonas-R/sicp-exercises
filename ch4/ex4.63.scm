;; run (initialize-data-base bible-database) in regular scheme to initialize

(define bible-database 
  '((son Adam Cain)
    (son Cain Enoch)
    (son Enoch Irad)
    (son Irad Mehujael)
    (son Mehujael Methushael)
    (son Methushael Lamech)
    (wife Lamech Ada)
    (son Ada Jabal)
    (son Ada Jubal)

    (rule (grandson ?gparent ?gson)
	  (and (son ?gparent ?parent)
	       (son ?parent ?gson)))
    (rule (son ?p ?son)
	  (and (wife ?p ?wife)
	       (son ?wife ?son)))))

(grandson cain ?s)
(son lamech ?s)
(grandson methushael ?s)
