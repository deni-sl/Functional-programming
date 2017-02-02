#lang racket

(define(divisible? n k)
  (if (= (remainder n k) 0)
      #t
      #f))


(define (number-divisors n)
  (define (help sum i)
    (cond
      [(> i n) sum]
      [(divisible? n i) (help (+ sum 1) (+ i 1))]
      [(help sum (+ i 1))]))
  (help 0 1))