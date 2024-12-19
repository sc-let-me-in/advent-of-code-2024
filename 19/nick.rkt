#lang racket

(require rackunit)
(require "../input.rkt")
(require "../nickutil.rkt")

(define in (input 19))

(define example #<<eof
r, wr, b, g, bwu, rb, gb, br

brwrr
bggr
gbbr
rrbgbr
ubwu
bwurrg
brgr
bbrgwb
eof
  )

(define (parse in)
  (match-let ([(list towels designs) (string-split in "\n\n")])
    (values (list->set (string-split towels ", ")) (string-split designs "\n"))))

(define/memoized (design-count design towels)
  (if (string=? design "")
      1
      (for/sum ([end-index (in-range (string-length design) 0 -1)]
                #:when (set-member? towels (substring design 0 end-index)))
        (design-count (substring design end-index (string-length design)) towels))))

(define (p1 in)
  (define-values (towels designs) (parse in))
  (for/sum ([design designs])
    (min 1 (design-count design towels))))

(check-equal? (p1 example) 6)
(p1 in)

(define (p2 in)
  (define-values (towels designs) (parse in))
  (for/sum ([design designs])
    (design-count design towels)))

(check-equal? (p2 example) 16)
(p2 in)
