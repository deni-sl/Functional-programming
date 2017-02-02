#lang racket

(define (number-of-digits n)
  (define (iter counter k)
    (cond
      [( = k 0) counter]
      [else (iter (+ counter 1) (quotient k 10))]))
  (iter 0 n))
    
(define (convert-to-decimal x k )
  (define (helper res power n)
    (cond
      [(= power (number-of-digits x)) res]
      [else (helper (+ res (* (remainder n 10) (expt k power))) (+ power 1) (quotient n 10)) ]))
  (helper 0 0 x))

(define (convert-to-wanted-base x n)
  (define (helper k res)
    (cond
      [(= k 0) res]
      [else (helper (quotient k n) (cons (remainder k n) res))]))
  (helper x (list)))

(define (from-list-to-number list)
  (define (helper l counter res)
    (cond
      [(> counter (length list)) res]
      [else (helper (rest l) (+ counter 1) (+ (* res 10) (first l)))]))
  (helper list 1 0))


(define (convert x k n)
  (from-list-to-number (convert-to-wanted-base (convert-to-decimal x k)  n) ))

;;tests
(convert 123 10 2)
(convert 173 8 10)
