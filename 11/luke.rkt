#lang racket

(require rackunit)
(require "../input.rkt")

(define in (input 11))

(require rackunit)
(require "../input.rkt")

(define example "0 1 10 99 999\n")

;; does one iteration
(define (iterate-stones stone-list)
    (if (empty? stone-list) '()
        (let* ([sn (car stone-list)] [slen (string-length sn)] [normalize (compose number->string string->number)])
            (cond 
                [(zero? (string->number sn)) (cons "1" (iterate-stones (cdr stone-list)))]
                [(and (natural? slen) (even? slen)) (cons (normalize (substring sn 0 (/ slen 2)))
                                                            (cons (normalize (substring sn (/ slen 2))) (iterate-stones (cdr stone-list))))]
                [else (cons (number->string (* 2024 (string->number sn))) (iterate-stones (cdr stone-list)))]))))

(define (repeater n l)
    (if (not (positive-integer? n)) l (repeater (- n 1) (iterate-stones l)))
)

(check-equal? (iterate-stones (string-split example)) '("1" "2024" "1" "0" "9" "9" "2021976"))
(check-equal? (repeater 1 '("125" "17")) '("253000" "1" "7"))
(check-equal? (repeater 2 '("125" "17")) '("253" "0" "2024" "14168"))
(check-equal? (repeater 3 '("125" "17")) '("512072" "1" "20" "24" "28676032"))


(print (length (repeater 25 (string-split (input 11)))))

;; this is an ez money DP solution i think???
;(print (length (repeater 75 (string-split (input 11)))))

