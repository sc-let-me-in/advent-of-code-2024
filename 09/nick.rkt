#lang racket

(require rackunit)
(require "../input.rkt")
(require "../nickutil.rkt")

(define in (string-trim	(input 9)))

(define example "2333133121414131402")

(define char->number (compose string->number string))

(define (disk-blocks in)
  (for/fold ([result '()] #:result (reverse result))
            ([n in] [id (sequence-add-between (in-naturals) #f)])
    (append (make-list (char->number n) id) result)))

(define (move-blocks blocks)
  (define-values (only-free only-files) (partition not blocks))
  (for/fold ([result '()] [files (reverse only-files)]  #:result (append (reverse result) only-free))
            ([block (take blocks (length only-files))] )
    (if block
        (values (cons block result) files)
        (values (cons (first files) result) (rest files)))))

(define (checksum blocks)
  (for/sum ([(n i) (in-indexed blocks)] #:when n) (* n i)))

(define p1 (compose checksum move-blocks disk-blocks))

(check-equal? (p1 example) 1928)
(p1 in)

(define (disk-blocks-grouped in)
  (for/vector ([n in] [id (sequence-add-between (in-naturals) #f)] #:unless (char=? n #\0))
    (list id (char->number n))))

(define (vector-find vec stop proc)
  (for/first ([(val idx) (in-indexed (in-vector vec 0 stop))] #:when (proc val)) idx))

(define (vector-swap! vec i1 i2)
  (let ([v1 (vector-ref vec i1)] [v2 (vector-ref vec i2)])
    (vector-set*! vec i2 v1 i1 v2)
    vec))

(define (defrag-block-groups blocks)
  (let loop ([file-idx (sub1 (vector-length blocks))] [blocks blocks])
    (define file (vector-ref blocks file-idx))
    (cond
      ; gone through whole list
      [(= file-idx 0) blocks]
      ; skip free space
      [(not (first file)) (loop (sub1 file-idx) blocks)]
      [else
       (define free-idx
         (vector-find blocks file-idx (λ (p) (and (not (first p)) (>= (second p) (second file))))))
       (define free (and free-idx (vector-ref blocks free-idx)))
       (match* (free file)
         ; file too big for any space
         [(#f _) (loop (sub1 file-idx) blocks)]
         ; skip empty, move to next
         [(_ (list #f _)) (loop (sub1 file-idx) blocks)]
         ; defrag file is exact size as empty
         [((list #f free-size) (list _ file-size))
          #:when (= free-size file-size)
          (loop (sub1 file-idx) (vector-swap! blocks free-idx file-idx))]
         ; defrag file is smaller than empty
         [((list #f free-size) (list _ file-size))
          (begin
            (vector-set! blocks free-idx (list #f file-size))
            (vector-swap! blocks free-idx file-idx)
            (define-values (l r) (vector-split-at blocks (add1 free-idx)))
            (loop file-idx (vector-append l (vector (list #f (- free-size file-size))) r)))])])))

(define (flatten-block-groups blocks)
  (for*/foldr ([result '()]) ([(id count) (sequence-values blocks)])
    (append (make-list count (or id 0)) result)))

(define p2 (compose checksum flatten-block-groups defrag-block-groups disk-blocks-grouped))

(check-eq? (p2 example) 2858)
(p2 in)
