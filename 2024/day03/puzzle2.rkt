#lang racket

(define (sum-mul-instructions str)
  (let* ([context-pattern #px"(^|do\\(\\)).*?(don't\\(\\)|$)"]
         [pattern #px"mul\\(([0-9]{1,3}),([0-9]{1,3})\\)"])
    (for*/sum ([sstr (regexp-match* context-pattern str)]
               [factors (regexp-match* pattern sstr #:match-select rest)])
      (apply * (map string->number factors)))))

(define (solve path)
  (let*-values ([(memory) (file->string path)])
    (sum-mul-instructions memory)))

(solve "input.txt")
