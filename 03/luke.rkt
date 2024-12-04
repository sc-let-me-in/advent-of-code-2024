#lang racket


(require "../input.rkt")
;; gonna copy PL on this one >:)

(define (calc-mul-expr expr)
    (foldl * 1 (map string->number (regexp-match* #rx"[0-9]+" expr))))

;; get mul exprs with regex (ugh hacky ik)
(foldl + 0 (map calc-mul-expr (regexp-match* #px"mul\\(-?[0-9]+,-?[0-9]+\\)" (input 3))))
