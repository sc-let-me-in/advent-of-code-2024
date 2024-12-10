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

(define (parse-line in)
    (let ([split (string-split in ":")])
        (cons (string->number (car split)) (map string->number (string-split (cadr split))))))

;; determine if one line is legal, to be mapped over the list of lines
(define (is-legal? line)
    (let ([revved (reverse (cdr line))])
        (let loop ([remaining (car line)] [operands revved])
            ;; possibilities
            (cond
                [(empty? (cdr operands)) (if (= (car operands) remaining) (car line) 0)]
                [((compose not zero? modulo) remaining (car operands)) (loop (- remaining (car operands)) (cdr operands))]
                [else (max (loop (/ remaining (car operands)) (cdr operands))
                           (loop (- remaining (car operands)) (cdr operands)))]
            ))))


;;;;;; TESTS

(define ex-lines (string-split example "\n"))
(check-equal? (parse-line (car ex-lines)) '(190 10 19))

(define first-test-line (parse-line (car ex-lines)))
(check-equal? (is-legal? first-test-line) 190)

(define should-be-false-test (parse-line (caddr ex-lines)))
(check-equal? (is-legal? should-be-false-test) 0)

(define mapped-parsed (map parse-line ex-lines))
(check-equal? (apply + (map is-legal? mapped-parsed)) 3749)

(define in-prepped (map parse-line (string-split (input 7) "\n")))
(print (apply + (map is-legal? in-prepped)))
