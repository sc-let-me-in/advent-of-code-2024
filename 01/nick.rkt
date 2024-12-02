#lang racket

(require rackunit)
(require "../input.rkt")

(define example #<<eof
3   4
4   3
2   5
1   3
3   9
3   3
eof
  )
(define in (input 1))

(define (parse in)
  (map (compose (curry map string->number) string-split)
       (string-split in "\n")))

(define (p1 in)
  (let-values ([(left right) (apply values (apply map (compose (curryr sort <) list) (parse in)))])
    (foldl (λ (l r acc) (+ acc (abs (- l r)))) 0 left right)))

(check-equal? (p1 example) 11)
(p1 in)

(define (p2 in)
  (let*-values ([(left right) (apply values (apply map list (parse in)))]
                [(counts) (foldl (λ (val acc) (hash-update acc val add1 0)) (hash) right)])
    (foldl (λ (val acc) (+ acc (* val (hash-ref counts val 0)))) 0 left)))

(check-equal? (p2 example) 31)
(p2 in)
