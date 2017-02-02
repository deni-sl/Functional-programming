#lang racket

(define (maximize list x)
  (define (helper lst max max-funct)
    (cond
      [(empty? lst) max-funct]
      [(> (abs ((first lst) x)) max) (helper (rest lst) (abs ((first lst) x)) ((first lst) x))]
      [else (helper (rest lst) max max-funct)]))
  (helper list 0 0))


;;tests
(maximize (list (λ (x) (- x 10)) (λ (x) (- x 5))) 5)
(maximize (list (λ (x) (- x 10)) (λ (x) (- x 5))) 9)