#lang racket


(define(divisible? n k)
  (if (= (remainder n k) 0)
      #t
      #f))
      
(define (number-of-divisors n)
  (define (sum k s)
    (cond
      [(> k n) s]
      [(divisible? n k) (sum (+ k 1) (+ s 1))]
      [else (sum (+ k 1) s)]))
  (sum 1 0))

(define (prime? n)
  (if (= (number-of-divisors n) 2)
      #t
      #f))

(define (sum-prime n k)
  (define (sum-p counter m s)
    (cond
      [(> counter n) s]
      [(prime? m) (sum-p (+ counter 1) (+ m 1) (+ s m))]
      [else (sum-p counter (+ m 1) s)]))
  (sum-p 1 k 0))