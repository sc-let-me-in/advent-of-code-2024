#lang racket

(require rackunit)
(require "../input.rkt")

(define in (input 20))

(define example #<<eof
###############
#...#...#.....#
#.#.#.#.#.###.#
#S#...#.#.#...#
#######.#.#.###
#######.#.#...#
#######.#.###.#
###..E#...#...#
###.#######.###
#...###...#...#
#.#####.#.###.#
#.#...#.#.#...#
#.#.#.#.#.#.###
#...#...#...###
###############
eof
  )

(define (parse in)
  (for*/hash ([(row y) (in-indexed (string-split in "\n"))] [(char x) (in-indexed row)])
    (values (make-rectangular x y) char)))

(define (find-pos maze char)
  (for/first ([(pos cur) (in-hash maze)] #:when (char=? cur char)) pos))

(define (find-path maze)
  (define end (find-pos maze #\E))
  (let search ([pos (find-pos maze #\S)] [prev #f])
    (cond [(= pos end) (list pos)]
          [else (for/first ([next (map (curry + pos) (list 1 -i -1 +i))]
                            #:unless (equal? next prev)
                            #:unless (char=? (hash-ref maze next) #\#))
                  (cons pos (search next pos)))])))

(define (dist p1 p2)
  (+ (abs (- (real-part p1) (real-part p2)))
     (abs (- (imag-part p1) (imag-part p2)))))

(define (find-cheats path cheat-len saved)
  (set-count
   (for*/set ([(cheat-start start-dist) (in-indexed (reverse path))]
              [(cheat-end end-dist) (in-indexed (reverse path))]
              #:do [(define cheat-dist (dist cheat-start cheat-end))]
              #:when (<= cheat-dist cheat-len)
              #:when (>= (- start-dist end-dist cheat-dist) saved))
     (cons cheat-start cheat-end))))

(define (p1 in saved)
  (find-cheats (find-path (parse in)) 2 saved))

(check-equal? (p1 example 1) 44)
(p1 in 100)

(define (p2 in saved)
  (find-cheats (find-path (parse in)) 20 saved))

(check-equal? (p2 example 50) 285)
(p2 in 100)
