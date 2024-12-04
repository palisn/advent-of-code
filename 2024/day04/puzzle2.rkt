#lang racket

(define (vref v . pos)
  (cond [(empty? pos) v]
        [else (apply vref (vector-ref v (first pos)) (rest pos))]))

;; gets word-search as a list of lines (strings)
(define (count-xmas-occurences word-search)
  (let ([ws (list->vector (map (compose list->vector string->list)
                               word-search))]
        [is-mas? (curry regexp-match? #rx"MAS|SAM")])
    (for*/sum ([i (in-range 1 (sub1 (vector-length ws)))]
               [j (in-range 1 (sub1 (vector-length ws)))]
               #:when (char=? (vref ws i j) #\A))
      (if (and (and (is-mas? (string (vref ws (sub1 i) (sub1 j))
                                     (vref ws i j)
                                     (vref ws (add1 i) (add1 j)))))
               (and (is-mas? (string (vref ws (sub1 i) (add1 j))
                                     (vref ws i j)
                                     (vref ws (add1 i) (sub1 j))))))
          1
          0))))

(define (solve path)
  (let*-values ([(word-search) (file->lines path)])
    (count-xmas-occurences word-search)))

(solve "input.txt")
