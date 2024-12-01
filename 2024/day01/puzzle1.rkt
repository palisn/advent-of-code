#lang racket

;; function that simplify input handling
(require 2htdp/batch-io
         srfi/1)

;; other imports
(require math/base)

(define (list-distance xs ys)
  (let ([xs (sort xs <)]
        [ys (sort ys <)])
    (sum (map (compose abs (curry apply -))
              (zip xs ys)))))

(define (solve path)
  (let*-values ([(input) (read-words-and-numbers/line path)]
                [(xs ys) (unzip2 input)])
    (list-distance xs ys)))

(solve "input.txt")
