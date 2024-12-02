#lang racket

(require rackunit)

(provide (all-defined-out))

;; returns a sliding list of lists window of size n
(define (window n xs)
  (define (window-acc xs acc)
    (if (< (length xs) n)
        (reverse acc)
        (window-acc (rest xs) (cons (take xs n) acc))))
  (window-acc xs '()))

(check-equal? (window 1 '(1 2 3 4)) '((1) (2) (3) (4)))
(check-equal? (window 2 '(1 2 3 4)) '((1 2) (2 3) (3 4)))
(check-equal? (window 3 '(1 2 3 4)) '((1 2 3) (2 3 4)))
(check-equal? (window 4 '(1 2 3 4)) '((1 2 3 4)))
