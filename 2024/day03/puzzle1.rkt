#lang racket

(define (sum-mul-instructions str)
  (let ([pattern #px"mul\\(([0-9]{1,3}),([0-9]{1,3})\\)"])
    (for/sum ([factors (regexp-match* pattern str #:match-select rest)])
      (apply * (map string->number factors)))))

(define (solve path)
  (let*-values ([(memory) (file->string path)])
    (sum-mul-instructions memory)))

(solve "input.txt")
