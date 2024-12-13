#lang racket

(require rackunit)
(require "../input.rkt")
(require "../nickutil.rkt")

(define in (input 12))

(define example #<<eof
RRRRIICCFF
RRRRIICCCF
VVRRRCCFFF
VVRCCCJFFF
VVVVCJJCFE
VVIVCCJJEE
VVIIICJJEE
MIIIIIJJEE
MIIISIJEEE
MMMISSJEEE
eof
  )

(define (parse in)
  (for*/hash ([(row y) (in-indexed (string-split in "\n"))] [(char x) (in-indexed row)])
    (values (make-rectangular x y) char)))

(define dirs (list 1 +i -1 -i))

(define (find-region plots start)
  (let find-region ([pos start] [seen (set)])
    (define char (hash-ref plots pos))
    (define next (for*/list ([dir dirs]
                             #:do [(define next-pos (+ dir pos))]
                             #:when (and (eq? char (hash-ref plots next-pos #f))
                                         (not (set-member? seen next-pos))))
                   next-pos))
    (if (empty? next)
        (set-add seen pos)
        (foldl find-region (set-add seen pos) next))))

(define (find-regions plots)
  (let find-regions ([regions '()] [positions (list->set (hash-keys plots))])
    (if (set-empty? positions)
        regions
        (let ([region (find-region plots (set-first positions))])
          (find-regions (cons region regions) (set-subtract positions region))))))

(define (region-stats region)
  (let* ([edges (for*/set ([pos region]
                           [dir dirs]
                           #:unless (set-member? region (+ pos dir)))
                  (list pos dir))]
         [sides (for*/sum ([(pos dir) (sequence-values edges)]
                           #:unless (set-member? edges (list (+ pos (* dir +i)) dir)))
                  1)])
    (values (set-count region) (set-count edges) sides)))

(define (sum-regions in)
  (for/fold ([p1 0] [p2 0]) ([region (find-regions (parse in))])
    (let-values ([(area perimeter sides) (region-stats region)])
      (values (+ p1 (* area perimeter)) (+ p2 (* area sides))))))

(let-values ([(p1 p2) (sum-regions example)])
  (check-equal? p1 1930)
  (check-equal? p2 1206))
(sum-regions in)
