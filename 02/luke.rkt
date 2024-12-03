#lang racket

(require "../input.rkt")

(define (safe-report? report comp)
    (let loop ([acc #t] [l (cdr report)] [prev (car report)])
        (if (empty? l) acc (loop (and acc (comp prev (car l))) (cdr l) (car l)))))
    

(define (gcomp x y)
    (and (> x y)
        (let ([diff (- x y)])
            (< diff 4))))

(define (lcomp x y)
    (and (< x y)
        (let ([diff (- y x)])
            (< diff 4))))

;; get the answer
(foldl 
    (lambda (x y) (if x (+ y 1) y))
    0
    (map (lambda (rep) (or (safe-report? rep gcomp) (safe-report? rep lcomp))) 
        (map (lambda (l) (map string->number l)) 
            (map string-split (string-split (input 2) "\n")))))
