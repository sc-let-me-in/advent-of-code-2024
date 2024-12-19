#lang racket

(require rackunit)
(require "../input.rkt")
(require "../nickutil.rkt")

(define in (input 14))

(define example #<<eof
p=0,4 v=3,-3
p=6,3 v=-1,-3
p=10,3 v=-1,2
p=2,0 v=2,-1
p=0,0 v=1,3
p=3,0 v=-2,-2
p=7,6 v=-1,-3
p=3,0 v=-1,-2
p=9,3 v=2,3
p=7,3 v=-1,2
p=2,4 v=2,-3
p=9,5 v=-3,-3
eof
  )

(define regex (pregexp #<<eof
p=(-?\d+),(-?\d+) v=(-?\d+),(-?\d+)
eof
                       ))

(define (parse in)
  (for/list ([match (regexp-match* regex in #:match-select rest)])
    (match-let ([(list x y dx dy) (map string->number match)])
      (list (make-rectangular x y) (make-rectangular dx dy)))))

(define (wrap n w h)
  (make-rectangular (modulo (real-part n) w) (modulo (imag-part n) h)))

(define (calculate pos vel secs w h)
  (wrap (+ pos (* vel secs)) w h))

(define (print-positions positions w h)
  (for* ([y (in-range h)] [x (in-range w)])
    (display (if (set-member? positions (make-rectangular x y)) "█" " ") (current-error-port))
    (when (= x (sub1 w)) (displayln "" (current-error-port)))))

(define (p1 in w h)
  (define (quad pos) (list (< (quotient w 2) (real-part pos)) (< (quotient h 2) (imag-part pos))))
  (for/fold ([counts (hash)] #:result (apply * (hash-values counts)))
            ([(pos vel) (sequence-values (parse in))]
             #:do [(define new-pos (calculate pos vel 100 w h))]
             #:unless (= (quotient w 2) (real-part new-pos))
             #:unless (= (quotient h 2) (imag-part new-pos)))
    (hash-update counts (quad new-pos) add1 0)))

(check-equal? (p1 example 11 7) 12)
(p1 in 101 103)

(define (p2 in w h)
  (define robots (parse in))
  (define counts
    (for/hash ([secs (in-range (* w h))])
      (define positions (for/set ([(pos vel) (sequence-values robots)])
                          (calculate pos vel secs w h)))
      (values (set-count positions) (cons positions secs))))
  (match-let ([(cons positions secs) (hash-ref counts (apply max (hash-keys counts)))])
    (print-positions positions w h)
    secs))

(p2 in 101 103)
