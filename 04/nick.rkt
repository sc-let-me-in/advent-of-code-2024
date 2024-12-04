#lang racket

(require rackunit)
(require "../input.rkt")

(define in (input 4))

(define example1 #<<eof
..X...
.SAMX.
.A..A.
XMAS.S
.X....
eof
  )

(define example2 #<<eof
MMMSXXMASM
MSAMXMSMSA
AMXSXMAAMM
MSAMASMSMX
XMASAMXAMM
XXAMMXXAMA
SMSMSASXSS
SAXAMASAAA
MAMMMXMMMM
MXMXAXMASX
eof
  )

(define example2* #<<eof
....XXMAS.
.SAMXMS...
...S..A...
..A.A.MS.X
XMASAMX.MM
X.....XA.A
S.S.S.S.SS
.A.A.A.A.A
..M.M.M.MM
.X.X.XMASX
eof
  )

(define (count-xmas str)
  (+ (length (regexp-match* #rx"XMAS" str)) (length (regexp-match* #rx"SAMX" str))))

(define (string->matrix str)
  (map string->list (string-split str "\n")))

(define (transpose lol)
  (apply map list lol))

(define (matrix->string mat)
  (string-join (map list->string mat) "\n"))

(define (rotate str)
  (matrix->string (transpose (string->matrix str))))

(define (diag str flip?)
  (define exploded (string->matrix str))
  (define spaces (build-list (length exploded) (curryr make-list #\space)))
  (define-values (lop rop) (if flip? (values identity reverse) (values reverse identity)))
  (matrix->string (transpose (map append (lop spaces) exploded (rop spaces)))))

(define (p1 in)
  (+ (count-xmas in)             ; -
     (count-xmas (rotate in))    ; |
     (count-xmas (diag in #t))   ; \
     (count-xmas (diag in #f)))) ; /

(check-equal? (p1 example1) 4)
(check-equal? (p1 example2) 18)
(check-equal? (p1 example2*) 18)
(p1 in)

(define example3 #<<eof
M.S
.A.
M.S
eof
  )

(define example4 #<<eof
.M.S......
..A..MSMS.
.M.S.MAA..
..A.ASMSM.
.M.S.M....
..........
S.S.S.S.S.
.A.A.A.A..
M.M.M.M.M.
..........
eof
  )

(define (p2 in)
  (define exploded (string->matrix in))
  (for*/sum ([x (in-range 1 (sub1 (length exploded)))]
             [y (in-range 1 (sub1 (length (first exploded))))])
    (define (check-coord char fx fy) (eq? char (list-ref (list-ref exploded (fy y)) (fx x))))
    (if (and (check-coord #\A identity identity)
             (or (and (check-coord #\M sub1 sub1)
                      (check-coord #\S add1 add1))
                 (and (check-coord #\S sub1 sub1)
                      (check-coord #\M add1 add1)))
             (or (and (check-coord #\M add1 sub1)
                      (check-coord #\S sub1 add1))
                 (and (check-coord #\S add1 sub1)
                      (check-coord #\M sub1 add1))))
        1 0)))

(check-equal? (p2 example3) 1)
(check-equal? (p2 example2) 9)
(check-equal? (p2 example4) 9)
(p2 in)
