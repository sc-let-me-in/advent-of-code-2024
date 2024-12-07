#lang racket

(require rackunit)
(require "../input.rkt")

(define in (input 7))

(define example #<<eof
190: 10 19
3267: 81 40 27
83: 17 5
156: 15 6
7290: 6 8 6 15
161011: 16 10 13
192: 17 8 14
21037: 9 7 18 13
292: 11 6 16 20
eof
  )

(define (parse in)
  (map (compose (curry map string->number)
                (curryr string-split #px":\\s|\\s"))
       (string-split in "\n")))

(define (concat l r)
  (inexact->exact (+ (* l (expt 10 (add1 (floor (log r 10))))) r)))

(define (valid-calibration? calibration concat?)
  (define test-val (first calibration))
  (define ops (if concat? (list + * concat) (list + *)))
  (for/fold ([possibilities (list (second calibration))]
             #:result (member test-val possibilities))
            ([num (drop calibration 2)])
    (for*/list ([op ops] [possible possibilities] #:when (< possible test-val))
      (op possible num))))

(define (sum-valid-calibrations calibrations concat?)
  (for/sum ([calibration calibrations]
            #:when (valid-calibration? calibration concat?))
    (first calibration)))

(define p1 (compose (curryr sum-valid-calibrations #f) parse))
(check-equal? (p1 example) 3749)
(p1 in)

(define p2 (compose (curryr sum-valid-calibrations #t) parse))
(check-equal? (p2 example) 11387)
(p2 in)
