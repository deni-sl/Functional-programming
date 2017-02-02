#lang racket

(define (gcd x y)
  (cond
    [(= x y) x]
    [(< x y) (gcd x (- y x))]
    [(> x y) (gcd (- x y) y)]))