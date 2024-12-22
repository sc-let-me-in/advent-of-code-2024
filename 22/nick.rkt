#lang racket

(require racket/hash)
(require rackunit)
(require "../input.rkt")

(define in (input 22))

(define example1 #<<eof
1
10
100
2024
eof
  )

(define example2 #<<eof
1
2
3
2024
eof
  )

(define (parse in)
  (for/list ([num (string-split in "\n")]) (string->number num)))

(define mix bitwise-xor)
(define prune (curryr modulo 16777216))
(define (next1 n) (prune (mix n (* n 64))))
(define (next2 n) (prune (mix n (quotient n 32))))
(define (next3 n) (prune (mix n (* n 2048))))
(define next (compose next3 next2 next1))

(define (secret-prices secret)
  (for/fold ([secret secret] [changes '()] [prices (hash)] #:result (values secret prices))
            ([_ (in-range 2000)])
    (let* ([prev-price (modulo secret 10)]
           [secret (next secret)]
           [price (modulo secret 10)]
           [changes (cons (- price prev-price) (if (= (length changes) 4) (take changes 3) changes))]
           [prices (if (and (= (length changes) 4) (not (hash-has-key? prices changes)))
                       (hash-set prices changes price)
                       prices)])
      (values secret changes prices))))

(define (p1 in)
  (for/sum ([secret (parse in)])
    (let-values ([(secret _) (secret-prices secret)])
      secret)))

(check-equal? (p1 example1) 37327623)
(p1 in)

(define (p2 in)
  (for/fold ([prices (hash)] #:result (apply max (hash-values prices)))
            ([secret (parse in)])
    (let-values ([(_ p) (secret-prices secret)])
      (hash-union p prices #:combine +))))

(check-equal? (p2 example2) 23)
(p2 in)
