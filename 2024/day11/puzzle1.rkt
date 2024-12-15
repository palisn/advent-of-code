#lang racket

(define (stone-count stones n)
  (let rec ([stones stones] [n n])
    (cond
      [(= n 0) (length stones)]
      [else
       (rec (append-map
             (λ (x)
               (cond
                 [(= x 0) (list 1)]
                 [(let ([len (add1 (truncate (log x 10)))])
                    (and (even? len)
                         (list (quotient x (expt 10 (/ len 2)))
                               (modulo x (expt 10 (/ len 2))))))]
                 [else (list (* 2024 x))]))
             stones)
            (sub1 n))])))

(define (solve path)
  (let*-values ([(stones) (file->list path)])
    (stone-count stones 25)))

(solve "input.txt")
