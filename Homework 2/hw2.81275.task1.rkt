#lang racket

(define (from-list-to-number list)
    (define (helper l counter res)
      (cond
        [(> counter (length list)) res]
        [else (helper (rest l) (+ counter 1) (+ (* res 10) (first l)))]))
    (helper list 1 0))


(define (sum-numbers str) 
   (define (helper lst counter num sum)
     (cond
       [(= counter  (+ (string-length str) 2)) sum]
       [(string->number (substring lst counter (+ counter 1))) (helper lst (+ counter 1) (append (list(string->number (substring lst counter (+ counter 1)))) num) sum)]
       [else (helper lst (+ counter 1) '() (+ sum (from-list-to-number (reverse num))))]))
   (helper (string-append str "aa") 0 (list) 0))

;;tests
(sum-numbers "a123b2c56")
(sum-numbers "a1b2c3")