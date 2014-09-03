(load "huffman.scm")

(define pairs '((A 2) (NA 16) (BOOM 1) (SHA 3) (GET 2) (YIP 9) (JOB 2) (WAH 1)))

(define tree (generate-huffman-tree pairs))

(display (encode '(Get a job Sha na na na na na na na na Get a job Sha na na na na na na na na Wah yip yip yip yip yip yip yip yip yip) tree))
