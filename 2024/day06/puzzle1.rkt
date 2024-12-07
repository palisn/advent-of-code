#lang racket

(define (patrol-path lab)
  (define-values (w h) (values (length (list-ref lab 0))
                               (length lab)))
  (define pos
    (let* ([chars '(#\<   #\^ #\>    #\v)]
           [dirs  '(left up right down)]
           [rows (map (curryr index-where (curryr member chars)) lab)]
           [y (index-where rows identity)]
           [x (list-ref rows y)]
           [dir (let ([char (list-ref (list-ref lab y) x)])
                  (list-ref dirs (index-of chars char)))])
      (list x y dir)))
  (define/match (turn pos)
    [(`(,x ,y ,dir))
     (list x y (match dir
                 ['up 'right]
                 ['right 'down]
                 ['down 'left]
                 ['left 'up]))])
  (define/match (next-pos pos)
    [(`(,x ,y ,dir))
     (let ([npos (match dir
                   ['up    (list x (sub1 y) dir)]
                   ['down  (list x (add1 y) dir)]
                   ['left  (list (sub1 x) y dir)]
                   ['right (list (add1 x) y dir)])])
       (match npos
         [`(,x ,y ,_)
          (if (and (and (x . < . w) (0 . <= . x))
                   (and (y . < . h) (0 . <= . y)))
              npos
              #f)]))])
  (define (move pos)
    (let ([npos (next-pos pos)])
      (match npos
          [`(,x ,y ,_)
           (if (not (char=? (list-ref (list-ref lab y) x) #\#))
               npos
               (move (turn pos)))]
           [_ #f])))
  (set-count
   (let rec ([pos pos] [path (set (take pos 2))])
        (cond
          [(move pos) => (λ (p)
                           (rec p (set-add path (take p 2))))]
          [else path]))))

(define (solve path)
  (let*-values ([(lab) (map string->list (file->lines path))])
    (patrol-path lab)))

(solve "input.txt")
