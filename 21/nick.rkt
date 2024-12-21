#lang racket

(require rackunit)
(require "../input.rkt")
(require "../nickutil.rkt")

(define in (input 21))

(define example #<<eof
029A
980A
179A
456A
379A
eof
  )

(define num-keypad (hash #\7 0+0i #\8 1+0i #\9 2+0i
                         #\4 0+1i #\5 1+1i #\6 2+1i
                         #\1 0+2i #\2 1+2i #\3 2+2i
                         #|    |# #\0 1+3i #\A 2+3i))

(define dir-keypad (hash #|    |# #\^ 1+0i #\A 2+0i
                         #\< 0+1i #\v 1+1i #\> 2+1i))

(define (path keypad from to)
  (let* ([from-pos (hash-ref keypad from)]
         [to-pos (hash-ref keypad to)]
         [dydx (- to-pos from-pos)]
         [h (make-string (abs (real-part dydx)) (if (negative? (real-part dydx)) #\< #\>))]
         [v (make-string (abs (imag-part dydx)) (if (negative? (imag-part dydx)) #\^ #\v))]
         [moving-right? (positive? (real-part dydx))]
         [prefer-pos (if moving-right?
                         (make-rectangular (real-part from-pos) (imag-part to-pos))
                         (make-rectangular (real-part to-pos) (imag-part from-pos)))]
         [use-prefer? (member prefer-pos (hash-values keypad))])
    ; moving right, prefer VH if possible
    ; moving left, prefer HV if possible
    (if (xor moving-right? use-prefer?)
        (string-append h v "A")
        (string-append v h "A"))))

(define/memoized (count-dir-presses code n)
  (if (= n 0)
      (string-length code)
      (for/sum ([(from to) (sequence-values (window 2 (cons #\A (string->list code))))])
        (count-dir-presses (path dir-keypad from to) (sub1 n)))))

(define (count-presses code n)
  (for/sum ([(from to) (sequence-values (window 2 (cons #\A (string->list code))))])
    (count-dir-presses (path num-keypad from to) n)))

(define (complexity code n)
  (* (count-presses code n)
     (string->number (second (regexp-match #px"0*(\\d+).*" code)))))

(define (p1 in)
  (for/sum ([code (string-split in "\n")])
    (complexity code 2)))

(check-equal? (p1 example) 126384)
(p1 in)

(define (p2 in)
  (for/sum ([code (string-split in "\n")])
    (complexity code 25)))

(p2 in)
