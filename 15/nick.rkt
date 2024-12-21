#lang racket

(require rackunit)
(require "../input.rkt")

(define in (input 15))

(define example #<<eof
##########
#..O..O.O#
#......O.#
#.OO..O.O#
#..O@..O.#
#O#..O...#
#O..O..O.#
#.OO.O.OO#
#....O...#
##########

<vv>^<v^>v>^vv^v>v<>v^v<v<^vv<<<^><<><>>v<vvv<>^v^>^<<<><<v<<<v^vv^v>^
vvv<<^>^v^^><<>>><>^<<><^vv^^<>vvv<>><^^v>^>vv<>v<<<<v<^v>^<^^>>>^<v<v
><>vv>v^v^<>><>>>><^^>vv>v<^^^>>v^v^<^^>v^^>v^<^v>v<>>v^v^<v>v^^<^^vv<
<<v<^>>^^^^>>>v^<>vvv^><v<<<>^^^vv^<vvv>^>v<^^^^v<>^>vvvv><>>v^<<^^^^^
^><^><>>><>^^<<^^v>>><^<v>^<vv>>v>>>^v><>^v><<<<v>>v<v<v>vvv>^<><<>^><
^>><>^v<><^vvv<^^<><v<<<<<><^v<<<><<<^^<v<^^^><^>>^<v^><<<^>>^v<v^v<v^
>^>>^v>vv>^<<^v<>><<><<v<<v><>v<^vv<<<>^^v^>^^>>><<^v>>v^v><^^>>^<>vv^
<><^^>^^^<><vvvvv^v<v<<>^v<v>v<<^><<><<><<<^^<<<^<<>><<><^^^>^^<>^>v<>
^^>vv<^v^v<vv>^<><v<^v>^^^>>>^^vvv^>vvv<>>>^<^>>>>>^<<^v>^vvv<>^<><<v>
v^^>>><<^^<>>^v^<v^vv<>v^<<>^<^v^v><^<<<><<^<v><v<>vv>>v><v^<vv<>v^<<^
eof
  )

(define (parse in)
  (match-let ([(list board dirs) (string-split in "\n\n")])
    (values
     (for*/hash ([(row y) (in-indexed (string-split board "\n"))] [(char x) (in-indexed row)])
       (values (make-rectangular x y) char))
     (map (match-λ [#\^ -i] [#\v +i] [#\> 1] [#\< -1])
          (string->list (string-replace dirs "\n" ""))))))

(define (try-move board pos dir)
  (match (hash-ref board pos)
    [#\# #f]
    [#\. board]
    [(and box (or #\[ #\]))
     #:when (= 0 (real-part dir))
     (let* ([dir* (match box [#\[ 1] [#\] -1])]
            [box* (match box [#\[ #\]] [#\] #\[])]
            [next (try-move board (+ pos dir) dir)]
            [next* (and next (try-move next (+ pos dir dir*) dir))])
       (and next* (hash-set* next* (+ pos dir*) #\. (+ pos dir) box (+ pos dir dir*) box*)))]
    [(and movable (or #\@ #\O #\[ #\]))
     (let ([next (try-move board (+ pos dir) dir)])
       (and next (hash-set* next (+ pos dir) movable pos #\.)))]))

(define (sum-boxes board)
  (for/sum ([(pos char) (in-hash board)] #:when (or (char=? char #\O) (char=? char #\[)))
    (+ (* 100 (imag-part pos)) (real-part pos))))

(define (print-board board)
  (define x-max (apply max (map real-part (hash-keys board))))
  (define y-max (apply max (map imag-part (hash-keys board))))
  (for* ([y (in-inclusive-range 0 y-max)] [x (in-inclusive-range 0 x-max)])
    (display (hash-ref board (make-rectangular x y)) (current-error-port))
    (when (= x-max x) (displayln "" (current-error-port)))))

(define (p1 in)
  (define-values (board dirs) (parse in))
  (for/fold ([board board]
             #:result (begin (when #f (print-board board)) (sum-boxes board)))
            ([dir dirs])
    (or (try-move board (for/first ([(pos char) (in-hash board)] #:when (char=? char #\@)) pos) dir)
        board)))

(check-equal? (p1 example) 10092)
(p1 in)

(define (p1-to-p2 in)
  (string-replace
   (string-replace
    (string-replace
     (string-replace
      in "#" "##")
     "." "..")
    "O" "[]")
   "@" "@."))

(define p2 (compose p1 p1-to-p2))

(check-equal? (p2 example) 9021)
(p2 in)
