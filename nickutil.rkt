#lang racket

(require rackunit)
(require lang/posn)

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

(define (posn-op op p1 p2)
  (make-posn (op (posn-x p1) (posn-x p2))
             (op (posn-y p1) (posn-y p2))))

(define posn-sub (curry posn-op -))
(define posn-add (curry posn-op +))

;; memoize any function definition
(define-syntax-rule (define/memoized (f args ...) body ...)
  (define f
    (let ([cache (make-hash)])
      (λ (args ...) (hash-ref! cache (list args ...) (thunk body ...))))))
