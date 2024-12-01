#lang racket

(define lines (file->lines "01/nick.txt"))
(define parsed (map (λ (l) (map string->number (string-split l))) lines))
(define-values (left right) (apply values (apply map list parsed)))
(println (foldl (λ (l r acc) (+ acc (abs (- l r)))) 0 (sort left <) (sort right <)))
(println (foldl (λ (v acc) (+ acc (* v (count (curry = v) right)))) 0 left))
