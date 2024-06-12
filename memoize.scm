#lang racket

;; By default, Racket doesn't have set-car! and set-cdr! functions.  The
;; following line allows us to use those:
(require r5rs)
;; Unfortunately, it does this by making cons return a "mutable pair"
;; instead of a "pair", which means that many built-ins may fail
;; mysteriously because they expect a pair, but get a mutable pair.
;; Re-define a few common functions in terms of car and friends, which
;; the above line make work with mutable pairs.
(define first car)
(define rest cdr)
(define second cadr)
(define third caddr)
(define fourth cadddr)
;; We also tell DrRacket to print mutable pairs using the compact syntax
;; for ordinary pairs.
(print-as-expression #f)
(print-mpair-curly-braces #f)

(define (last lst)
  (if (or (null? lst) (null? (rest lst))) lst (last (rest lst))))

"Problem 1"
(define (make-table)
  (list 'table))
(define (table? table)
  (and (pair? table) (eq? 'table (first table))))
(define (table-put! table key value)
  (if (table? table)
      (set-cdr! (last table) (list (list key value)))
      (error "Not a table")))
(define (table-has-key? table key)
  (define (checker lst)
    (cond
      [(null? lst) #f]
      [(eq? (first (first lst)) key) #t]
      [else (checker (rest lst))]))
  (if (table? table) (checker (rest table)) (error "Not a table")))
(define (table-get table key)
  (define (helper lst)
    (if (null? lst)
        (error "No key")
        (let ([row (first lst)])
          (if (eq? key (first row)) (second row) (helper (rest lst))))))
  (if (table? table) (helper (rest table)) (error "Not a table")))

"Problem 2"
(define (fib n)
  (if (< n 2)
      n
      (+ (fib (- n 1)) (fib (- n 2)))))

;; make-monitored
(define (make-monitored f)
  (let ([count 0])
    (lambda (x)
      (cond
        [(equal? x 'how-many-calls?) count]
        [(equal? x 'reset-call-count) (set! count 0)]
        [else
         (begin
           (set! count (+ count 1))
           (f x))]))))

#|
(set! fib (make-monitored fib))

(fib 8) ;; => 21
(fib 'how-many-calls?) ;; => 67
(fib 8) ;; => 21
(fib 'how-many-calls?) ;; => 134
(fib 'reset-call-count)
(fib 'how-many-calls?) ;; => 0


(define mon-fib (make-monitored fib))
(mon-fib 8)
(mon-fib 'how-many-calls?) ;; => 1
#|
reason why the result is 1
(define (mon-fib n)
  (if (< n 2)
      n
      (+ (fib (- n 1)) (fib (- n 2)))))
|#
|#

"Problem 3"

;; make-num-calls-table
(define (make-num-calls-table f max)
  (let ([table (make-table)])
    (define (helper arg)
      (if (> arg max)
          table
          (begin
            (f arg)
            (table-put! table arg (f 'how-many-calls?))
            (f 'reset-call-count)
            (helper (+ 1 arg)))))
    (helper 1)))

#|
(set! fib (make-monitored fib))
(make-num-calls-table fib 10)
; -> (table (1 1) (2 3) (3 5) (4 9) (5 15) (6 25) (7 41) (8 67) (9 109) (10 177))
|#

"Problem 4"

;; memoize
(define (memoize f)
  (let ([table (make-table)])
    (lambda (x)
      ; (display (list table x (table-has-key? table x)))
      ; (newline)
      (if (table-has-key? table x)
          (table-get table x)
          (let ([val (f x)])
            (table-put! table x val)
            val)))))

#|
(set! fib (memoize fib))
(fib 8) ;; => 21
|#

"Problem 5 (optional)"

;; advise
(define (advise func before after)
  (lambda (x)
    (begin
      (if (not (null? before)) (before))
      (let ([val (func x)])
        (if (not (null? after)) (after))
        val))))

#|
(define (add-1 x)
  (+ x 1))
(define advised-add-1
  (advise add-1
          (lambda () (displayln "calling add-1"))
          (lambda () (displayln "add-1 done"))))
(advised-add-1 5)
|#

"Problem 6 (optional)"

;; make-monitored-with-advice
(define (make-monitored-with-advice f)
  (let ([count 0])
    (lambda (x)
      (define helper (advise f '() (lambda () (set! count (+ count 1)))))
      (let ([result (helper x)])
        (display "Num calls: ")
        (displayln count)
        result))))

(set! fib (make-monitored-with-advice fib))
(fib 10)
 

;; Allow this file to be included from elsewhere, and export all defined
;; functions from it.
(provide (all-defined-out))
