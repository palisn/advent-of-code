#lang racket

(define (patrol-path lab_)
  (define-values (w h) (values (length (list-ref lab_ 0))
                               (length lab_)))
  (define pos
    (let* ([chars '(#\<   #\^ #\>    #\v)]
           [dirs  '(left up right down)]
           [rows (map (curryr index-where (curryr member chars)) lab_)]
           [y (index-where rows identity)]
           [x (list-ref rows y)]
           [dir (let ([char (list-ref (list-ref lab_ y) x)])
                  (list-ref dirs (index-of chars char)))])
      (list x y dir)))
  (define lab (list->vector (map list->vector lab_)))
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
         (if (not (char=? (vector-ref (vector-ref lab y) x) #\#))
             npos
             (move (turn pos)))]
        [_ #f])))
  ;; check if guard on `pos` loops infinitely in `lab`
  (define (loop? pos)
    (let rec ([pos pos] [path (set pos)])
      (cond
        [(move pos)
         => (λ (p)
              (if (set-member? path p)
                  #t
                  (rec p (set-add path p))))]
        [else #f])))

  ;; try to put obstacle on every legal part of the guard path
  (set-count
   (let rec ([pos pos] [path (set (take pos 2))])
     (let ([npos (move pos)])
       (match npos
         [`(,x ,y ,dir)
          (let ([found (rec npos (set-add path (take npos 2)))]
                [p (take npos 2)])
            (vector-set! (vector-ref lab y) x #\#)
            (begin0
                (if (and (not (set-member? path p))
                         (loop? pos))
                    (set-add found p)
                    found)
              (vector-set! (vector-ref lab y) x #\.)))]
         [else (set)])))))

(define (solve path)
  (let*-values ([(lab) (map string->list (file->lines path))])
    (patrol-path lab)))

(solve "input.txt")
