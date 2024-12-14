#lang racket

(define (at vec x . xs)
  (cond
    [(null? xs) (vector-ref vec x)]
    [else (apply at (vector-ref vec x) xs)]))

(define (@ vec p) (apply at vec (reverse p)))

(define (rate _map)
  (define-values (width height) (values (vector-length (vector-ref _map 0))
                                        (vector-length _map)))
  (define/match (in-bounds p)
    [(`(,x ,y)) (and (x . >= . 0) (x . < . width)
                     (y . >= . 0) (y . < . height))])
  (define/match (neighbors p)
    [(`(,x ,y))
     (list (list (sub1 x) y) (list (add1 x) y)
           (list x (sub1 y)) (list x (add1 y)))])
  (define trailheads
    (for*/list ([x (in-range width)]
                [y (in-range height)]
                #:when (= (_map . at . y x) 0))
      (list x y)))

  ;; sum of ratings
  (for/sum ([th trailheads])
    (let rec ([ps (list th)] [n 0])
      (cond
        [(= n 9) (length ps)]
        [else
         (rec (filter (compose (curry = (add1 n)) (curry @ _map))
                      (filter in-bounds (append-map neighbors ps)))
              (add1 n))]))))


(define (solve path)
  (let*-values ([(topo-map) (list->vector
                             (map (λ (line)
                                    (list->vector
                                     (map (compose string->number string) (string->list line))))
                                  (file->lines path)))])
    (rate topo-map)))

(solve "input.txt")
