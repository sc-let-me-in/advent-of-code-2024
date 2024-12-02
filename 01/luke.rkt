#lang racket

(require "../input.rkt")

;; parse-input : returns the two lists formed by the input, sorted
(define (parse-input input-string-port)
    (let parse-line ([line (read-line input-string-port)]
                     [first-list '()]
                     [second-list '()])
        (if (not (eof-object? line))
            (let ([items (string-split line)])
                (parse-line (read-line input-string-port) 
                            (cons (string->number (car items)) first-list) 
                            (cons (string->number (second items)) second-list)))
            (cons (sort first-list <) (sort second-list <)))))

(define (get-distance l1 l2 acc)
    (if (empty? l1) acc
        (get-distance (cdr l1) (cdr l2) (+ acc (abs (- (car l1) (car l2)))))))

(let ([lists (parse-input (open-input-string (input 1)))])
    (get-distance (car lists) (cdr lists) 0))