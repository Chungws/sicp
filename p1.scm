#lang sicp

(define (fold-right func init lst)
  (if (null? lst) init (func (car lst) (fold-right func init (cdr lst)))))

; Problem 1
(define (integral func num-steps x1 x2)
  (define (rect a b)
    (* (func a) (- b a)))
  (let ([delta (if (= x2 x1) 0.0 (* 1.0 (/ (- x2 x1) num-steps)))])
    (if (< num-steps 0)
        0
        (+ (rect x1 (+ x1 delta))
           (integral func (- num-steps 1) (+ x1 delta) x2)))))
(define area (integral (lambda (x) x) 10000 0 10)) ; -> 49.994999999999955

; Problem 2
(define (approx-pi num-steps)
  (define (area x)
    (sqrt (- 1 (* x x))))
  (* 4 (integral area num-steps 0 1)))
(approx-pi 1000000) ; -> 3.141594652413842

; Problem 3
(define (rectangle func x1 x2)
  (* (func x1) (- x2 x1)))
(define (trapezoid func x1 x2)
  (+ (rectangle func x1 x2) (/ (* (- x2 x1) (- (func x2) (func x1))) 2)))
(define (integral-with piece func num-steps x1 x2)
  (let ([delta (if (= x2 x1) 0.0 (* 1.0 (/ (- x2 x1) num-steps)))])
    (if (< num-steps 0)
        0
        (+ (piece func x1 (+ x1 delta))
           (integral-with piece func (- num-steps 1) (+ x1 delta) x2)))))
(define area-with
  (integral-with rectangle (lambda (x) x) 10000 0 10)) ; -> 49.994999999999955
(integral-with trapezoid (lambda (x) x) 100 0 10)
(equal? area area-with) ; -> #t

; Problem 4
(define (better-pi num-steps)
  (define (area x)
    (sqrt (- 1 (* x x))))
  (* 4 (integral-with trapezoid area num-steps 0 1)))
(better-pi 1000000) ; -> 3.1415926524137543

(define (deriv-constant wrt constant)
  0)

; Problem 5
(define (deriv-variable wrt var)
  (if (eq? wrt var) 1 0))

(deriv-variable 'x 3) ; -> 0
(deriv-variable 'x 'x) ; -> 1

; Problem 6
(define (derivative wrt expr)
  (cond
    [(number? expr) (deriv-constant wrt expr)]
    [(symbol? expr) (deriv-variable wrt expr)]
    [(and (list? expr) (eq? '+ (car expr))) (deriv-sum wrt expr)]
    [(and (list? expr) (eq? '* (car expr))) (deriv-product wrt expr)]
    [else (error "Don't know how to differentiate" expr)]))

(derivative 'x 3) ; -> 0
(derivative 'x 'x) ; -> 1

; Problem 7
(define (deriv-sum wrt expr)
  (let ([exprs (cdr expr)])
    (cons
     '+
     (fold-right (lambda (x acc) (cons (derivative wrt x) acc)) '() exprs))))
(deriv-sum 'x '(+ x 2)) ; -> (+ 1 0)
(derivative 'x '(+ x 2)) ; -> (+ 1 0)

; Problem 8
(define (deriv-product wrt expr)
  (let ([exprs (cdr expr)]
        [second (car (cdr expr))]
        [third (car (cdr (cdr expr)))])
    (list '+
          (list '* second (derivative wrt third))
          (list '* (derivative wrt second) third))))
(deriv-product 'x '(* x 3)) ; -> (+ (* x 0) (* 1 3))
(derivative 'x '(* x 3)) ; -> (+ (* x 0) (* 1 3))
