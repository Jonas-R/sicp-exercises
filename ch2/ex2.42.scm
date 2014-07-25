; 8 queens puzzle

(define (queens board-size)
  (define (queen-cols k)
    (if (= k 0)
	(list empty-board)
	(filter
	 (lambda (positions) (safe? k positions))
	 (flatmap
	  (lambda (rest-of-queens)
	    (map (lambda (new-row)
		   (adjoin-position new-row k rest-of-queens))
		 (enumerate-interval 1 board-size)))
	  (queen-cols (- k 1))))))
  (queen-cols board-size))

; board representation
(define (make-coordinate row col) (cons row col))
(define (get-row coord) (car coord))
(define (get-col coord) (cdr coord))

(define (adjoin-position row col board)
  (append board (list (make-coordinate row col))))
(define empty-board '())

(define (safe? k board)
  (define coord1 (list-ref board (- k 1)))
  ;test if two queens (coord1 and coord2) are safe against each other
  (define (safe-helper coord2)
    (not (or (= (get-row coord1) (get-row coord2)) ;same row
	     (= (get-col coord1) (get-col coord2)) ;same col
	     (= (abs (- (get-row coord1) (get-row coord2)))
		(abs (- (get-col coord1) (get-col coord2))))))) ;same diagonal
  (fold-right and-l #t (map safe-helper (take (- k 1) board))))

; you can't pass regular and as an argument WHAT?
(define and-l (lambda (a b) (and a b)))

; return the first n elements of l
(define (take n l)
  (if (= n 0)
      '()
      (cons (car l) (take (- n 1) (cdr l)))))
