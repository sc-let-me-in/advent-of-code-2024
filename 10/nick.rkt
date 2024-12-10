#lang racket

(require lang/posn)
(require racket/hash)
(require rackunit)
(require "../input.rkt")
(require "../nickutil.rkt")

(define in (input 10))

(define example #<<eof
89010123
78121874
87430965
96549874
45678903
32019012
01329801
10456732
eof
  )

(define (parse in)
  (for*/hash ([(row y) (in-indexed (string-split in "\n"))] [(n x) (in-indexed row)])
    (values (make-posn x y) (string->number (string n)))))

(define dirs (list (make-posn 0 1) (make-posn 1 0) (make-posn 0 -1) (make-posn -1 0)))

(define (search tmap unique?)
  (define-values (store add size)
    (if unique? (values set set-union set-count) (values list append length)))
  (for/sum ([(trailhead n) (in-hash tmap)] #:when (eq? n 0))
    (size
     (let search-pos ([pos trailhead] [n 0])
       (cond [(= n 9) (store pos)]
             [else (for/fold ([results (store)])
                             ([next-pos (map (curry posn-add pos) dirs)]
                              #:do [(define next-n (hash-ref tmap next-pos #f))]
                              #:when (eq? next-n (add1 n)))
                     (add (search-pos next-pos next-n) results))])))))

(define (p1 in)
  (search (parse in) #t))

(check-equal? (p1 example) 36)
(p1 in)

(define (p2 in)
  (search (parse in) #f))

(check-equal? (p2 example) 81)
(p2 in)
