#lang racket

(require rackunit)
(require "../input.rkt")
(require "../nickutil.rkt")

(define in (input 11))

(define example "125 17")

(define (parse in)
  (map string->number (string-split in)))

(define digits (compose add1 floor (curryr log 10)))

(define/memoized (blink stone n)
  (cond
    [(= n 0) 1]
    [(= stone 0) (blink 1 (sub1 n))]
    [(even? (digits stone))
     (let-values ([(l r) (quotient/remainder stone (expt 10 (quotient (digits stone) 2)))])
       (+ (blink l (sub1 n)) (blink r (sub1 n))))]
    [else (blink (* stone 2024) (sub1 n))]))

(define (count-blinks stones n)
  (for/sum ([count (sequence-map (curryr blink n) stones)]) count))

(define (p1 in)
  (count-blinks (parse in) 25))

(check-equal? (p1 example) 55312)
(p1 in)

(define (p2 in)
  (count-blinks (parse in) 75))

(p2 in)
