#lang racket

(require math/base)

(define (print-queue rules updates)
  (define (middle xs) (list-ref xs (quotient (length xs) 2)))
  (define (fn x acc)
    (let* ([correct? (first acc)]
           [pages (second acc)]
           [pages-after-x
            (map second (filter (compose (curry = x) first) rules))])
      (list (and correct? (not (set-member? pages x)))
            (set-union pages (list->set pages-after-x)))))
  (sum (map middle
            (filter (compose first (curry foldr fn (list #t (set))))
                    updates))))

(define (parse lines)
  (let ([rules-str (takef lines non-empty-string?)]
        [updates-str (drop (dropf lines non-empty-string?) 1)])
    (values (map (compose (curry map string->number)
                          (curryr string-split "|"))
                 rules-str)
            (map (compose (curry map string->number)
                          (curryr string-split ","))
                 updates-str))))

(define (solve path)
  (let*-values ([(input) (file->lines path)]
                [(ordering-rules updates) (parse input)])
    (print-queue ordering-rules updates)))

(solve "input.txt")
