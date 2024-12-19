#lang racket

(require data/queue)
(require rackunit)
(require "../input.rkt")

(define in (input 18))

(define example #<<eof
5,4
4,2
4,5
3,0
2,1
6,3
2,4
1,5
0,6
3,3
2,6
5,1
1,2
5,5
2,5
6,5
1,4
0,4
6,4
1,1
6,1
1,0
0,5
1,6
2,0
eof
  )

(define (parse in)
  (define pairs (for/list ([pair (string-split in "\n")])
                  (map string->number (string-split pair ","))))
  (values pairs (first (argmax first pairs)) (second (argmax second pairs))))

(define (search blocks w h)
  (let ([end (make-rectangular w h)]
        [queue (make-queue)]
        [seen (mutable-set)])
    (enqueue! queue (list 0))
    (set-add! seen 0)
    (do ([result #f])
      ((or (queue-empty? queue) result) result)
      (let* ([path (dequeue! queue)]
             [pos (first path)])
        (cond [(= pos end) (set! result (reverse path))]
              [else (for ([next (map (curry + pos) (list 1 -i -1 +i))]
                          #:unless (set-member? blocks next)
                          #:unless (set-member? seen next)
                          #:when (<= 0 (real-part next) w)
                          #:when (<= 0 (imag-part next) h))
                      (set-add! seen next)
                      (enqueue! queue (cons next path)))])))))

(define (p1 in count)
  (define-values (pairs w h) (parse in))
  (define blocks (list->set (map (curry apply make-rectangular) (take pairs count))))
  (sub1 (length (search blocks w h))))

(check-equal? (p1 example 12) 22)
(p1 in 1024)

(define (p2 in start)
  (define-values (pairs w h) (parse in))
  (define points (map (curry apply make-rectangular) pairs))
  (define initial (list->set (take points start)))
  (let find-block ([blocks initial]
                   [remaining (drop points start)]
                   [last-path (list->set (search initial w h))])
    (let* ([block (first remaining)]
           [blocks (set-add blocks block)]
           [remaining (rest remaining)])
      (cond [(set-member? last-path block)
             (define path (search blocks w h))
             (if path
                 (find-block blocks remaining (list->set path))
                 (~a (real-part block) "," (imag-part block)))]
            [else (find-block blocks remaining last-path)]))))

(check-equal? (p2 example 12) "6,1")
(displayln (p2 in 1024))
