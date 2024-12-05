#lang racket

(require rackunit)
(require "../input.rkt")

(define in (input 5))

(define example #<<eof
47|53
97|13
97|61
97|47
75|29
61|13
75|53
29|13
97|29
53|29
61|53
97|53
61|29
47|13
75|47
97|75
47|61
75|61
47|29
75|13
53|13

75,47,61,53,29
97,61,53,29,13
75,29,13
75,97,47,61,53
61,13,29
97,13,75,29,47
eof
  )

(define (parse in)
  (define-values (rules updates) (apply values (string-split in "\n\n")))
  (define (parse-nums str sep)
    (for/list ([nums (string-split str "\n")])
      (map string->number (string-split nums sep))))
  (values
   (parse-nums rules "|")
   (parse-nums updates ",")))

(define (rules-ht rules)
  (for/fold ([ht (hash)]) ([rule rules])
    (hash-update ht (first rule) (curry cons (second rule)) '())))

(define (right-order? update rules-ht)
  (for/fold ([seen '()] [valid? #t] #:result valid?)
            ([page update])
    (values
     (cons page seen)
     (and valid? (andmap (compose not (curryr member (hash-ref rules-ht page '()))) seen)))))

(define (fix-order update rules-ht)
  (sort update #:key (λ (p) (length (hash-ref rules-ht p '()))) >))

(define (sum-middle updates)
  (for/fold ([acc 0]) ([update updates])
    (+ acc (list-ref update (floor (/ (length update) 2))))))

(define (sum-updates in)
  (let*-values ([(rules updates) (parse in)]
                [(rules-ht) (rules-ht rules)]
                [(correct incorrect) (partition (curryr right-order? rules-ht) updates)])
    (values
     (sum-middle correct)
     (sum-middle (map (curryr fix-order rules-ht) incorrect)))))

(let-values ([(p1 p2) (sum-updates example)])
  (check-equal? p1 143)
  (check-equal? p2 123))
(sum-updates in)
