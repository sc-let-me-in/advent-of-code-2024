#lang racket

(require math)
(require rackunit)
(require "../input.rkt")

(define in (input 13))

(define example #<<eof
Button A: X+94, Y+34
Button B: X+22, Y+67
Prize: X=8400, Y=5400

Button A: X+26, Y+66
Button B: X+67, Y+21
Prize: X=12748, Y=12176

Button A: X+17, Y+86
Button B: X+84, Y+37
Prize: X=7870, Y=6450

Button A: X+69, Y+23
Button B: X+27, Y+71
Prize: X=18641, Y=10279
eof
  )

(define regex (pregexp #<<eof
Button A: X\+(\d+), Y\+(\d+)
Button B: X\+(\d+), Y\+(\d+)
Prize: X=(\d+), Y=(\d+)
eof
                       ))

(define (parse in)
  (for/list ([match (regexp-match* regex in #:match-select rest)])
    (map string->number match)))

(define (count-tokens machine valid? offset)
  (match-let*
      ([(list ax ay bx by px py) machine]
       [toks (matrix-solve (matrix [[ax bx] [ay by]]) (col-matrix [(+ offset px) (+ offset py)]))])
    (and (array-andmap valid? toks)
         (matrix-trace (matrix* toks (row-matrix [3 1]))))))

(define (p1 in)
  (for/sum ([machine (parse in)])
    (or (count-tokens machine (λ (press) (and (integer? press) (>= 100 press))) 0) 0)))

(check-equal? (p1 example) 480)
(p1 in)

(define (p2 in)
  (for/sum ([machine (parse in)])
    (or (count-tokens machine integer? 10000000000000) 0)))

(check-equal? (p2 example) 875318608908)
(p2 in)
