#lang racket

(define (nconcat . ns)
  (string->number
   (apply string-append (map number->string ns))))

(define (test-equation res nums)
  (set-member? (foldl
                (λ (x acc)
                  (for*/set ([y acc]
                             [op (list nconcat + *)])
                    (op y x)))
                (set (first nums))
                (drop nums 1))
               res))

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
