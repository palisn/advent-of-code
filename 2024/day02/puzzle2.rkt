#lang racket

;; functions that simplify input handling
(require 2htdp/batch-io
         srfi/1)

(define (safe? report)
  (let ([diff-safe? (λ (x y) (let ([diff (abs (- x y))])
                               (and (1 . <= . diff)
                                    (diff . <= . 3))))])
    (for*/or ([comb (in-combinations report (sub1 (length report)))]
              [comp (list <= >=)])
      (for/and ([x comb]
                [y (rest comb)])
        (and (diff-safe? x y)
             (comp x y))))))

(define (solve path)
  (let*-values ([(reports) (read-words-and-numbers/line path)])
    (count safe? reports)))

(solve "input.txt")
