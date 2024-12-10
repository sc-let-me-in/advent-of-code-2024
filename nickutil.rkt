#lang racket

(require rackunit)

(provide (all-defined-out))

;; returns a sliding list of lists window of size n
(define (window n xs)
  (let loop ([xs xs] [acc '()])
    (if (< (length xs) n)
        (reverse acc)
        (loop (rest xs) (cons (take xs n) acc)))))

(check-equal? (window 1 '(1 2 3 4)) '((1) (2) (3) (4)))
(check-equal? (window 2 '(1 2 3 4)) '((1 2) (2 3) (3 4)))
(check-equal? (window 3 '(1 2 3 4)) '((1 2 3) (2 3 4)))
(check-equal? (window 4 '(1 2 3 4)) '((1 2 3 4)))


;; "unzips" the elements of a list when iterating over a sequence of lists
(define (sequence-values seq) (sequence-map (curry apply values) seq))
