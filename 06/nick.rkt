#lang racket

(require lang/posn)
(require rackunit)
(require "../input.rkt")

(define in (input 6))

(define example #<<eof
....#.....
.........#
..........
..#.......
.......#..
..........
.#..^.....
........#.
#.........
......#...
eof
  )

(define (parse in)
  (list->vector (map (compose list->vector string->list) (string-split in "\n"))))

(define (find-start board)
  (for/or ([y (in-range (vector-length board))] [row board])
    (let ([x (vector-member #\^ row)])
      (and x (make-posn x y)))))

(define/match (next-pos pos dir)
  [((posn x y) 'up)    (make-posn x (sub1 y))]
  [((posn x y) 'down)  (make-posn x (add1 y))]
  [((posn x y) 'left)  (make-posn (sub1 x) y)]
  [((posn x y) 'right) (make-posn (add1 x) y)])

(define/match (rotate dir)
  [('up) 'right]
  [('right) 'down]
  [('down) 'left]
  [('left) 'up])

(define (set-block board pos)
  (vector-set/copy board (posn-y pos) (vector-set/copy (vector-ref board (posn-y pos)) (posn-x pos) #\#)))

(define (update-path path pos dir)
  (hash-set path pos (set-add (hash-ref path pos (set)) dir)))

(define (count-positions in)
  (define board (parse in))
  (define start-pos (find-start board))
  (let loop ([board board]
             [pos start-pos]
             [dir 'up]
             [paths (hash start-pos (set 'up))]
             [obstructions (set)])
    (define next (next-pos pos dir))
    (cond
      ; are we in a loop?
      [(set-member? (hash-ref paths next (set)) dir) #t]
      ; are we moving off the board (termination condition/no loop)?
      [(not (and (<= 0 (posn-x next) (sub1 (vector-length board)))
                 (<= 0 (posn-y next) (sub1 (vector-length (vector-ref board 0))))))
       (if (set? obstructions) (values (hash-count paths) (set-count obstructions)) #f)]
      ; are we moving into a block?
      [(eq? #\# (vector-ref (vector-ref board (posn-y next)) (posn-x next)))
       (loop board pos (rotate dir) (update-path paths pos (rotate dir)) obstructions)]
      ; does placing an obstruction put us in a loop?
      [(and (set? obstructions)                   ; disabled for nesting
            (not (set-member? obstructions next)) ; don't check already known obstructions
            (not (hash-has-key? paths next))      ; existing path can't be an obstruction
            (loop (set-block board next) pos (rotate dir) paths #f))
       (loop board next dir (update-path paths next dir) (set-add obstructions next))]
      ; otherwise add to path and continue
      [else (loop board next dir (update-path paths next dir) obstructions)])))

(let-values ([(p1 p2) (count-positions example)])
  (check-equal? p1 41)
  (check-equal? p2 6))
  
(count-positions in)
