#lang sicp

(define (map func lst)
  (if (null? lst) '() (cons (func (car lst)) (map func (cdr lst)))))
(define (for-each func lst)
  (if (not (null? lst))
      (begin
        (func (car lst))
        (for-each func (cdr lst)))))
(define (last-ptr lst)
  (if (null? (cdr lst)) lst (last-ptr (cdr lst))))

; Mutant pairs
(define y (list 'a 'b))
(define x (list y y))

; Q1
y ; -> (a b)

; Q2
x ; -> ((a b) (a b))

; Q3
; (c)
#|
(define x (cons 'x (cons 'x '())))
(define y '())
(let ((z (list 'a 'b)))
          (set-car! x z)
          (set-car! (cdr x) z)
          (set! y z))
|#

; Q4
(set-cdr! (cdr x) (cdr (car x)))
x ; -> ((a b) (a b) b)

; Get it together
(define foo (list 1 2 3))
(define bar (list 4 5 6))
(define (append! lst1 lst2)
  (let ([rear-ptr (last-ptr lst1)])
    (begin
      (set-cdr! rear-ptr lst2)
      lst1)))

(define baz (append! foo bar))
baz ; -> (1 2 3 4 5 6)
foo ; -> (1 2 3 4 5 6)

; Advantages : Time, Space , Disadvantages : Not easy to debug
(define foo2 (list 1 2 3))
(define bar2 (append! foo2 foo2))
bar2 ; -> #0=(1 2 3 . #0#) (Cycle)

; Coming or going?
(define (reverse! lst)
  (if (or (null? lst) (null? (cdr lst)))
      lst
      (let ((tmp (reverse! (cdr lst))))
        (display (list tmp lst))
        (newline)
        (set-cdr! (last-ptr tmp) lst)
        (display (list tmp lst))
        (newline)
        (set-cdr! lst '())
        (display (list tmp lst))
        (newline)
        tmp
        )))
(define foo3 (list 1 2 3 4))
(define bar3 (reverse! foo3))
foo3
bar3

; Stacking the deck
(define (make-stack)
  (list 'stack '()))

(define (stack? stack)
  (and (pair? stack) (eq? (car stack) 'stack)))

(define (empty-stack? stack)
  (if (stack? stack) (null? (car (cdr stack))) (error "Not stack")))

(define (push-stack! stack elem)
  (if (stack? stack)
      (set-cdr! stack (list (cons elem (car (cdr stack)))))
      (error "Not stack")))

(define (stack-top stack)
  (if (stack? stack) (caadr stack) (error "Not stack")))

(define (pop-stack! stack)
  (if (stack? stack)
      (if (empty-stack? stack)
          (error "Empty stack")
          (let ([top (stack-top stack)])
            (set-cdr! stack (list (cdadr stack)))
            top))
      (error "Not stack")))

#|
(define my-stack (make-stack))
(stack? my-stack) ; -> #t
(stack? 5) ; -> #f
(empty-stack? my-stack) ; -> #t
(push-stack! my-stack 'foo)
(push-stack! my-stack 'bar)
(empty-stack? my-stack) ; -> #f
(stack-top my-stack) ; -> bar
(pop-stack! my-stack) ; -> bar
(pop-stack! my-stack) ; -> foo
(empty-stack? my-stack) ; -> #t
(pop-stack! my-stack) ;=> ERROR
|#

; Shadowing
(define x1 1)
(define y1 2)
(define z1 3)
(define (foo5 x1)
  (define y1 50)
  (list x1 y1 z1))
(list x1 y1 z1) ; -> (1 2 3), x1 y1 z1 in global
(foo5 40) ; -> (40 50 3), x1 in foo5 is 40 (parameter), y1 in foo5 is 50
(set! x1 5) ; change global x1 to 5
(list x1 y1 z1) ; -> (5 2 3), x1 in global is 5
(foo5 45) ; -> (45 50 3), x1 in foo5 is 45 (parameter), y1 in foo5 is 50

; Simple local state
(define bar5
  (let ([result 'uninitialized])
    (lambda (x)
      (set! result (if (eq? result 'uninitialized) x (max result x)))
      result)))

(bar5 4) ; -> 4
(bar5 50) ; -> 50
(bar5 2) ; -> 50

; Accumulation anticipated
(define make-accumulator
  (lambda ()
    (let ([count 0])
      (lambda (increment)
        (set! count (+ count increment))
        count))))

(define a (make-accumulator))
(a 3) ; -> 3
(a 2) ; -> 5
(define b (make-accumulator))
(b 2) ; -> 2
(a 1) ; -> 6

; Next verse, same as the first?
(define make-accumulator2
  (let ([count 0])
    ;(display count)
    ;(newline)
    (lambda ()
      (lambda (increment)
        (set! count (+ count increment))
        count))))

; Already count initialized when define make-accumulator2.

; (display 'before-c-define)
; (newline)
(define c (make-accumulator2))
; (display 'after-c-define)
; (newline)

(c 3) ; -> 3
(c 2) ; -> 5
(define d (make-accumulator2))
(d 2) ; -> 7
(c 1) ; -> 8

; Bonus
(define (loops? lst)
  (if (or (null? lst) (null? (cdr lst)))
      #f
      
      #t))
(define safe (list 1 2 3))
(define uhoh (list 1 2 3))
(begin (append! uhoh uhoh) 'trap-set)

(loops? safe) ; -> #f
(loops? uhoh) ; -> #t
