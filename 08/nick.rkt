#lang racket

(require lang/posn)
(require rackunit)
(require "../input.rkt")
(require "../nickutil.rkt")

(define in (input 8))

(define example #<<eof
............
........0...
.....0......
.......0....
....0.......
......A.....
............
............
........A...
.........A..
............
............
eof
  )

(define (parse in)
  (for*/fold ([antenna-map (hash)]
              #:result (values antenna-map
                               (string-length (first (string-split in "\n")))
                               (length (string-split in "\n"))))
             ([(row y) (in-indexed (string-split in "\n"))]
              [(char x) (in-indexed row)]
              #:unless (char=? char #\.))
    (hash-set antenna-map char (cons (make-posn x y) (hash-ref antenna-map char '())))))

(define (antinode-count antenna-map width height make-antinodes)
  (for*/fold ([antinodes (set)]
              #:result (set-count antinodes))
             ([positions (hash-values antenna-map)]
              [(p1 p2) (sequence-values (combinations positions 2))])
    (set-union antinodes (make-antinodes p1 p2 width height))))

(define (posn-op op p1 p2)
  (make-posn (op (posn-x p1) (posn-x p2))
             (op (posn-y p1) (posn-y p2))))

(define posn-sub (curry posn-op -))
(define posn-add (curry posn-op +))

(define (p1-antinodes p1 p2 width height)
  (define dydx (posn-sub p2 p1))
  (list->set
   (filter (match-λ [(posn x y) (and (<= 0 x (sub1 width)) (<= 0 y (sub1 height)))])
           (list (posn-sub p1 dydx) (posn-add p2 dydx)))))

(define (p2-antinodes p1 p2 width height)
  (define m (match (posn-sub p2 p1) [(posn dx dy) (/ dy dx)]))
  (define b (- (posn-y p1) (* m (posn-x p1))))
  (list->set
   (filter (match-λ [(posn x y)
                     (and (integer? x) (integer? y) (<= 0 x (sub1 width)) (<= 0 y (sub1 height)))])
           (build-list width (λ (x) (make-posn x (+ (* m x) b)))))))

(define (p1 in)
  (define-values (antenna-map width height) (parse in))
  (antinode-count antenna-map width height p1-antinodes))

(check-equal? (p1 example) 14)
(p1 in)

(define (p2 in)
  (define-values (antenna-map width height) (parse in))
  (antinode-count antenna-map width height p2-antinodes))

(check-equal? (p2 example) 34)
(p2 in)
