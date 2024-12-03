#lang racket

(define (sum-mul-instructions str)
  (let ([pattern #px"mul\\(([0-9]{1,3}),([0-9]{1,3})\\)"])
    (for/sum ([mult (regexp-match* pattern str #:match-select rest)])
      (let ([a (string->number (first mult))]
            [b (string->number (second mult))])
        (* a b)))))

(define (solve path)
  (let*-values ([(memory) (file->string path)])
    (sum-mul-instructions memory)))

(solve "input.txt")
