#lang racket

(define lines (file->lines "01/nick.txt"))
(define parsed (map (compose (curry map string->number) string-split) lines))
(define-values (left right) (apply values (apply map (compose (curryr sort <) list) parsed)))
(define part-1 (foldl (λ (l r acc) (+ acc (abs (- l r)))) 0 left right))
(define part-2 (foldl (λ (v acc) (+ acc (* v (count (curry = v) right)))) 0 left))
part-1
part-2
