#lang racket

;; function that simplify input handling
(require 2htdp/batch-io
         srfi/1)

;; other imports
(require math/base)

(define (similarity-score xs ys)
  (let ([counts (map (λ (x) (count (curry = x) ys)) xs)])
    (sum (map (curry apply *) (zip xs counts)))))

(define (solve path)
  (let*-values ([(input) (read-words-and-numbers/line path)]
                [(xs ys) (unzip2 input)])
    (similarity-score xs ys)))

(solve "input.txt")
