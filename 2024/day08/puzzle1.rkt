#lang racket

(define (antinode-count arr)
  (define-values (width height) (values (vector-length (vector-ref arr 0))
                                        (vector-length arr)))
  
  (define antennas-by-frequency
    (for*/fold ([freqs (hash)])
               ([x (in-range width)]
                [y (in-range height)])
      (let ([freq (vector-ref (vector-ref arr y) x)])
        (cond
          [(char=? freq #\.) freqs]
          [(hash-has-key? freqs freq)
           (hash-set freqs freq (set-add (hash-ref freqs freq)
                                         (list x y)))]
          [else (hash-set freqs freq (set (list x y)))]))))
  
  (define/match (in-bounds p)
    [(`(,x ,y)) (and (and (0 . <= . x) (x . < . width))
                     (and (0 . <= . y) (y . < . height)))])
  
  (define (antinodes p q)
    (let ([diff (map - p q)])
      (filter in-bounds (list (map + p diff)
                              (map - q diff)))))
  
  (set-count
   (for*/set ([(freq locs) antennas-by-frequency]
              [l1 locs]
              [l2 locs]
              #:unless (equal? l1 l2)
              [antinode (antinodes l1 l2)])
     antinode)))


(define (solve path)
  (let*-values ([(arr) (list->vector
                        (map (compose list->vector string->list)
                             (file->lines path)))])
    (antinode-count arr)))

(solve "input.txt")
