#lang racket

(require rackunit)
(require "../input.rkt")

(define in (input 3))

(define example1 "xmul(2,4)%&mul[3,7]!@^do_not_mul(5,5)+mul(32,64]then(mul(11,8)mul(8,5))")

(define (mul-str-pair p)
  (apply * (map string->number p)))

(check-equal? (mul-str-pair '("2" "4")) 8)

(define r1 #px"mul\\((\\d+),(\\d+)\\)")

(define (p1 in)
  (let ([pairs (regexp-match* r1 in #:match-select rest)])
    (foldl (λ (p acc) (+ acc (mul-str-pair p))) 0 pairs)))

(check-equal? (p1 example1) 161)
(p1 in)

(define example2 "xmul(2,4)&mul[3,7]!^don't()_mul(5,5)+mul(32,64](mul(11,8)undo()?mul(8,5))")

(define r2 #px"don't\\(\\).*?do\\(\\)")

(define (p2 in)
  (p1 (regexp-replace* r2 in "")))

(check-equal? (p2 example2) 48)
(p2 in)

(define r2* #px"mul\\((\\d+),(\\d+)\\)|do\\(\\)|don't\\(\\)")

(define (p2* in)
  (define instrs (regexp-match* r2* in #:match-select (match-λ [(list d #f #f) d] [m (rest m)])))
  (define/match (exec-instr instr acc)
    [("don't()" (list _  acc)) (list #f acc)]
    [("do()"    (list _  acc)) (list #t acc)]
    [(p         (list #t acc)) (list #t (+ acc (mul-str-pair p)))]
    [(_         _            )  acc])
  (second (foldl exec-instr '(#t 0) instrs)))

(check-equal? (p2* example2) 48)
(check-equal? (p2* in) (p2 in))
