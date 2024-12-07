#lang racket

(define (test-equation res nums)
  (member res
          (foldl
           (λ (x acc)
             (append (map (curry + x) acc)
                     (map (curry * x) acc)))
           (list (first nums))
           (drop nums 1))))


(define (parse line)
  (match-let* ([`(,res ,nums) (string-split line ": ")]
               [res (string->number res)]
               [nums (map string->number (string-split nums))])
    (values res nums)))

(define (solve path)
  (let*-values ([(equations) (file->lines path)])
    (for/sum ([equation equations])
      (let-values ([(res nums) (parse equation)])
        (if (test-equation res nums)
            res
            0)))))

(solve "input.txt")
