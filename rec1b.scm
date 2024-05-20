#lang sicp

; Sum of all its parts
(define (sum-numbers-rec m n)
  (if (> m n) 0 (+ m (sum-numbers-rec (+ m 1) n))))
(sum-numbers-rec 1 5)

(define (sum-numbers-iter m n)
  (sum-numbers-iter-helper m n 0))
(define (sum-numbers-iter-helper curr last sum)
  (if (> curr last) sum (sum-numbers-iter-helper (+ curr 1) last (+ sum curr))))
(sum-numbers-iter 1 5)

; (sum-numbers 5 1) ; stack overflow

; Fibonacci
(define (fib-rec n)
  (cond
    [(< n 1) 0]
    [(= n 1) 1]
    [else (+ (fib-rec (- n 1)) (fib-rec (- n 2)))]))
(fib-rec 4) ; call 9 fib-rec

(define (fib-iter n)
  (define (fib-iter-helper counter prev now)
    (if (= counter 1) now (fib-iter-helper (- counter 1) now (+ prev now))))
  (if (= n 0) 0 (fib-iter-helper n 0 1)))
(fib-iter 4)

; Feel the power
(define (my-expt-rec x y)
  (if (= y 0) 1 (* x (my-expt-rec x (- y 1)))))
(my-expt-rec 2 10)

(define (my-expt-iter x y)
  (define (my-expt-iter-helper x y result)
    (if (= y 0) result (my-expt-iter-helper x (- y 1) (* x result))))
  (my-expt-iter-helper x y 1))
(my-expt-iter 2 10)

(define (fast-expt x y)
  (define (is_even? n)
    (= (modulo n 2) 0))
  (define (square n)
    (* n n))
  (if (= y 0)
      1
      (if (is_even? y) (square (fast-expt x (/ y 2))) (* x (square (fast-expt x (/ (- y 1) 2)))))))
(fast-expt 2 10)