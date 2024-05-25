#lang sicp

(define (map func lst)
  (if (null? lst) '() (cons (func (car lst)) (map func (cdr lst)))))

(define (filter predicate lst)
  (if (null? lst)
      '()
      (if (predicate (car lst))
          (cons (car lst) (filter predicate (cdr lst)))
          (filter predicate (cdr lst)))))

(define (fold-right func init lst)
  (if (null? lst) init (func (car lst) (fold-right func init (cdr lst)))))

(define (square x)
  (* x x))

; Flying first-class

; 1. divided-by
(define (divide-by num)
  (lambda (x) (/ x num)))

; ((divide-by 7) 49) ; -> 7

; 2. square-and-add
(define (square-and-add num)
  (lambda (x) (+ num (* x x))))

; ((square-and-add 2) 5) ; -> 27

; 3. compose
(define (compose f g)
  (lambda (x) (f (g x))))

; ((compose (lambda (x) (+ x 1)) (lambda (x) (* x 2))) 10) ;  -> 21

; consider this

; Q1.
; (1, 2)
; (1, (3, (5, nil)))
; (((3, 2), (1, 0)) nil)
; [0, 1, 2]
; [(1, 2), [4, 5], 3]

; Q2.
(list 1 2 3)
(cons 1 (cons 2 3))
(list (list 1 2) (list 3 4) (list 5 6))

; Q3.
((lambda (seq) (car (cdr (cdr (cdr seq))))) '(7 6 5 4 3 2 1))
((lambda (seq) (car (cdr (cdr (car (cdr seq)))))) '((7) (6 5 4) (3 2) 1))
((lambda (seq) (car (car (cdr (car (cdr (car (cdr seq))))))))
 '(7 (6 (5 (4 (3 (2 (1))))))))
((lambda (seq) (car (car (car (cdr (cdr (car (car (cdr seq)))))))))
 '(7 ((6 5 ((4)) 3) 2) 1))

; Down for the Count
(define (list-ref lst n)
  (if (= n 0) (car lst) (list-ref (cdr lst) (- n 1))))
; (list-ref '(1 2 3 4) 1) ; -> 2

; Copy cat
(define (copy lst)
  (map (lambda (x) x) lst))

(define L1 (list 1 5 (list 8 9) 'foo (quote bar)))
(eq? L1 (copy L1)) ; -> false
(eq? (copy L1) (copy L1)) ; -> false
(equal? L1 (copy L1)) ; -> true
(eq? (list-ref L1 2) (list-ref (copy L1) 2)) ; -> true

; Got it backwards
#|
(define (reverse lst)
  (if (null? lst)
      '()
      (append (reverse (cdr lst)) (list (car lst)))))
|#
#|
; ChatGPT (hard to understand)
(define (reverse lst)
  (define (cons-op x acc)
    (lambda (prev-result)
      (acc (cons x prev-result))))

  (define (identity x) x)

  ((fold-right cons-op identity lst) '()))
|#
(define (reverse lst)
  (fold-right (lambda (x y) (append y (list x))) '() lst))
(reverse '(1 2 3 4 5)) ; -> (5 4 3 2 1)
(reverse (list (list 1 2) (list 3 4) 5)) ; -> (5 (3 4) (1 2))

; A special snowflake
(define (unique lst)
  (define (include? seq val)
    (cond
      [(null? seq) #f]
      [(equal? (car seq) val) #t]
      [else (include? (cdr seq) val)]))
  (define (unique-add x acc)
    (begin
      (display acc)
      (newline)
      (if (include? acc x)
          (cons x (filter (lambda (el) (not (= el x))) acc))
          (cons x acc))))
  (fold-right unique-add `() lst))

(unique '(1 2 2 3 4 5 4 8)) ; -> (1 2 3 4 5 8)

; Getting things all set

(define (make-range min max label)
  (list 'range min max label))
(define (range-min range)
  (car (cdr range)))
(define (range-max range)
  (car (cdr (cdr range))))
(define (range-label range)
  (car (cdr (cdr (cdr range)))))

; Q1.
(define (range? thing)
  (if (list? thing) (equal? (car thing) 'range) #f))

(define L (make-range 1 4 'L))
(range? L) ; -> #t

; Q2.
(define (within-range? x range)
  (if (range? range) (and (< (range-min range) x) (> (range-max range) x)) #f))

(within-range? 11 L) ; -> #f

; Q3.
(define (make-set)
  (list 'set))
(define (set? thing)
  (if (list? thing) (equal? (car thing) 'set) #f))
(define (add-range-to-set r set)
  (cons 'set (cons r (set-ranges set))))
(define (set-ranges set)
  (cdr set))

(define S (make-set))
(set? S) ; -> #t
(define L2 (make-range 2 10 'L2))
(add-range-to-set L2
                  (add-range-to-set L
                                    S)) ; -> (set (range 2 10 L2) (range 1 4 L))
(define S2 (add-range-to-set L2 (add-range-to-set L S)))

; Q4.
(define (within? x thing)
  (define (check-ranges x ranges)
    (if (null? ranges)
        #f
        (or (within-range? x (car ranges)) (check-ranges x (cdr ranges)))))
  (define (check-set x set)
    (let ([ranges (set-ranges set)])
      (if (null? ranges) #f (check-ranges x ranges))))
  (cond
    [(range? thing) (within-range? x thing)]
    [(set? thing) (check-set x thing)]
    [else #f]))

(within? 2 L) ; -> #t
(within? 11 L) ; -> #f
(within? 2 S) ; -> #f
(within? 9 S2) ; -> #t
(within? 11 S2) ; -> #f

; Q5.
(define (labels-at x set)
  (if (not (within? x set))
      '()
      (map range-label
           (filter (lambda (range) (within? x range)) (set-ranges set)))))
(labels-at 3 S2)

; Bonus
; ( (lambda (x) (x x))  (lambda (x) (x x)) ) ; -> infinite loop
