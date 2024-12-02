#lang racket

(require rackunit)
(require "../input.rkt")
(require "../nickutil.rkt")

(define example #<<eof
7 6 4 2 1
1 2 7 8 9
9 7 6 2 1
1 3 2 4 5
8 6 4 4 1
1 3 6 7 9
eof
  )
(define in (input 2))

(define (parse in)
  (map (compose (curry map string->number) string-split)
       (string-split in "\n")))

;; check if list is sorted ascending/descending by diffs are within 3, nonzero
(define (safe? l)
  (let* ([diffs (map (match-lambda [(list l r) (- r l)]) (window 2 l))]
         [min (apply min diffs)]
         [max (apply max diffs)])
    (or (<= -3 min max -1) (<= 1 min max 3))))

(define (p1 in)
  (count safe? (parse in)))

(check-equal? (p1 example) 2)
(p1 in)

;; check if any sublists are safe by dropping 1 element
(define (safe-ish? l)
  (ormap safe? (combinations l (sub1 (length l)))))

(define (p2 in)
  (count safe-ish? (parse in)))

(check-equal? (p2 example) 4)
(p2 in)
