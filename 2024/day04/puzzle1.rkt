#lang racket

(require math/base
         point-free)

(define (transpose xss)
  (apply map list xss))

(define (rotate-c xss)
  (let loop ([xss xss] [n 1])
    
    (cond
      [(empty? xss) '()]
      [(empty? (first xss))
       (loop (rest xss) (- n 2))]
      [else
       (cons
        (reverse (map first (take xss n)))
        (loop (append (map rest (take xss n))
                      (drop xss n))
              (add1 n)))])))

(define/compose rotate-cc reverse rotate-c reverse)

;; gets word-search as a list of lines (strings)
(define (count-xmas-occurences word-search)
  (let ([ws (map string->list word-search)]
        [count-xmas (compose length
                             (curry regexp-match* "XMAS")
                             list->string)])
    (for/sum ([modifier (list identity transpose rotate-c rotate-cc)])
      (+ (sum (map count-xmas (modifier ws)))
         (sum (map count-xmas (map reverse (modifier ws))))))))

(define (solve path)
  (let*-values ([(word-search) (file->lines path)])
    (count-xmas-occurences word-search)))

(solve "input.txt")
