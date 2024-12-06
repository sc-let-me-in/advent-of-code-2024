#lang racket


(require "../input.rkt")

(define (calc-mul-expr expr)
    (foldl * 1 (map string->number (regexp-match* #rx"[0-9]+" expr))))

(define (exec-add expr active-status)
        (match expr
            [(regexp #rx"don't") (set-box! active-status 0) 0]
            [(regexp #rx"do") (set-box! active-status 1) 0]
            [(regexp #rx"mul") (* (unbox active-status) (calc-mul-expr expr))]
            [_ (error expr)]))


;; get mul exprs with regex (ugh hacky ik)
(define muls (regexp-match* #px"(mul\\(-?[0-9]+,-?[0-9]+\\))" (input 3)))
(define instrs (regexp-match* #px"(mul\\(-?[0-9]+,-?[0-9]+\\))|(do\\(\\))|(don't\\(\\))" (input 3)))

(foldl + 0 (map calc-mul-expr muls))

(let ([active-status (box 1)])
    (foldl (lambda (e n) (+ n (exec-add e active-status))) 0 instrs))
