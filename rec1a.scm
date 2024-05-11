#lang sicp

; Getting Started
42 ; 42
"Hello World" ; Hello World
; (8 + 9) ; error
(+ 8 9) ; 17
(define a 10)
a ; 10
; b ; error
(define b a)
b ; 10
(* a b) ; 100

; Nested Expressions
(* (- 8 4) (+ 1 10)) ; 44
(define foo 100)
(define bar (* 10 foo))
(+ (- (- 2010 (/ bar foo)) (* foo (- (/ bar foo) 3))) 37) ; 1337

; Hello, λ
(lambda (x) (/ x 1024)) ; procedure
((lambda (x) (/ x 1024)) 4096) ; 4
(lambda () 1) ; procedure
((lambda () 1)) ; 1
; ((lambda () 1) 5) ; error
(lambda (y z) (+ z y))
((lambda (y z) (+ z y)) 5 4) ; 9
; ((lambda (y z) (+ z y)) x 7) ; error

; What's in a name?
(define x 1)
(define y -1)
(define foo (lambda (a b) (+ a b)))
(define bar (lambda (x) x))
(define baz (lambda () 1))
(define quux (lambda (p) (foo p 5)))

x ; 1
foo ; procedure
(foo 1 2) ; 3
; (foo 1) ; error
; (foo) ; error
(baz) ; 1
(bar 10) ; 10
(quux (foo (baz) (bar y))) ; 5

; Short and sweet: Syntatic Sugar
(define foo1 (lambda (a b) (+ a b)))
(define (foo2 a b)
  (+ a b))
(foo1 1 2)
(foo2 1 2)

(define bar1 (lambda () 1))
(define (bar2)
  1)
(bar1)
(bar2)
