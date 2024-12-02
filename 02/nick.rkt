#lang racket

(require rackunit)
(require "../input.rkt")

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

;; brute force check if list is sorted assending/decending and diffs are witin 3, nonzero
(define (safe? l)
  (and (or (eq? (sort l <) l)
           (eq? (sort l >) l))
       (andmap (λ (curr prev) (< 0 (abs (- curr prev)) 4)) (take l (sub1 (length l))) (rest l))))

(define (p1 in)
  (count safe? (parse in)))

(check-equal? (p1 example) 2)
(p1 in)

;; brute force check if any sublists are safe by dropping 1
(define (safe-ish? l)
  (ormap safe? (for/list ([i (in-range (length l))])
                 (append (take l i) (drop l (add1 i))))))

(define (p2 in)
  (count safe-ish? (parse in)))

(check-equal? (p2 example) 4)
(p2 in)
