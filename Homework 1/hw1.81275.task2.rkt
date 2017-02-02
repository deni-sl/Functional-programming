#lang racket

(define (number-of-digits n)
  (define (iter counter k)
    (cond
      [( = k 0) counter]
      [else (iter (+ counter 1) (quotient k 10))]))
  (iter 0 n))

(define (reverse n)
  (define (help counter i k)
    (cond
      [(= counter (number-of-digits n)) i]
      [else (help (+ counter 1) (+ (* i 10) (remainder k 10)) (quotient k 10) )]))
  (help 0 0 n))

(define (palindrom a b)
  (define (help-pal x counter )
    (cond
      [(> x b) counter]
      [(= (reverse x) x) (help-pal (+ x 1) (+ counter 1))]
      [else (help-pal (+ x 1) counter)]))
  (help-pal a 0))

    