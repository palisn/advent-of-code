#lang racket

;; function that simplify input handling
(require 2htdp/batch-io
         srfi/1)

(define (safe? report)
  (let ([diff-safe? (λ (x y) (let ([diff (abs (- x y))])
                               (and (1 . <= . diff)
                                    (diff . <= . 3))))]
        [len (- (length report) 1)])
    ((disjoin (curry andmap (conjoin diff-safe? <=))
              (curry andmap (conjoin diff-safe? >=)))
     (take report len) (drop report 1))))

(define (solve path)
  (let*-values ([(reports) (read-words-and-numbers/line path)])
    (count safe? reports)))

(solve "input.txt")
