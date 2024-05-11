#lang sicp

; Problem 1
(define (bitfunc x)
  (+ (expt x 4) 4 (* -5 (expt x 2))))
(= (bitfunc 0) 4)
(= (bitfunc 1) 0)
(= (bitfunc 2) 0)
(= (bitfunc 3) 40)
(= (bitfunc 4) 180)

; Problem 2
(define (bitfunc-rect x1 x2)
  (* (bitfunc x1) (- x2 x1)))
(= (bitfunc-rect 1 2) 0)
(= (bitfunc-rect 2 3) 0)
(= (bitfunc-rect 3 4) 40)
(= (bitfunc-rect 0 1) 4)
(= (bitfunc-rect 0 0.5) 2)
(< (bitfunc-rect 1.5 2) 0)
(+ (bitfunc-rect 3 3.5) (bitfunc-rect 3.5 4))

; Problem 3
(define (bitfunc-integral-recur num-steps x1 x2)
  (define delta (if (= x2 x1) 0.0 (* 1.0 (/ (- x2 x1) num-steps))))
  (if (< num-steps 0)
      0
      (+ (bitfunc-rect x1 (+ x1 delta)) (bitfunc-integral-recur (- num-steps 1) (+ x1 delta) x2))))
(bitfunc-integral-recur 2 3 4)

(define (bitfunc-integral-iter num-steps x1 x2)
  (define delta (if (= x2 x1) 0.0 (* 1.0 (/ (- x2 x1) num-steps))))
  (define (iter-helper x sum)
    (if (> x (- x2 delta)) sum (iter-helper (+ x delta) (+ sum (bitfunc-rect x (+ x delta))))))
  (iter-helper x1 0))
(bitfunc-integral-iter 2 3 4)

; Problem 4
(define (bitfunc-integral-difference num-steps x1 x2)
  (abs (- (bitfunc-integral-recur num-steps x1 x2) (bitfunc-integral-iter num-steps x1 x2))))
(bitfunc-integral-difference 100 3 4)