#lang racket

(require data/union-find)

(define (at vec x . xs)
  (cond
    [(null? xs) (vector-ref vec x)]
    [else (apply at (vector-ref vec x) xs)]))

(define (@ vec p) (apply at vec (reverse p)))

(define (fence-prices farm)
  (define-values (width height) (values (vector-length (vector-ref farm 0))
                                        (vector-length farm)))
  (define/match (in-bounds p)
    [(`(,x ,y)) (and (x . >= . 0) (x . < . width)
                     (y . >= . 0) (y . < . height))])
  (define/match (neighbors p)
    [(`(,x ,y))
     (list (list (sub1 x) y) (list (add1 x) y)
           (list x (sub1 y)) (list x (add1 y)))])
  (define areas
    (let rec ([x 0] [y 0] [areas (list)])
      (cond
        [(x . >= . width) (rec 0 (add1 y) areas)]
        [(y . >= . height) areas]
        [(ormap (curryr set-member? (list x y)) areas)
         (rec (add1 x) y areas)]
        [else
         (rec x y
              (cons
               (let rec ([stack (list (list x y))]
                         [area (set)]
                         [char (at farm y x)])
                 (cond
                   [(empty? stack) area]
                   [else
                    (let ([pos (car stack)])
                      (cond
                        [(set-member? area pos)
                         (rec (cdr stack) area char)]
                        [(char=? (@ farm pos) char)
                         (rec (append (filter in-bounds (neighbors pos)) (cdr stack))
                              (set-add area pos)
                              char)]
                        [else
                         (rec (cdr stack) area char)]))]))
               areas))])))

  (for/sum ([area areas])
    (let* ([fields (set->list area)]
           [sides
            (for/sum ([dir '((1 0) (-1 0) (0 1) (0 -1))]
                      [selector (list first first second second)])
              (let* ([fields (filter (negate (curry set-member? area))
                                     (map (curry map + dir) fields))]
                     [sets (for/hash ([f fields] [n (in-naturals 0)])
                             (values f (uf-new n)))])
                (for* ([f1 fields] [f2 fields])
                  (when (member f1 (neighbors f2))
                    (uf-union! (hash-ref sets f1) (hash-ref sets f2))))
                (length
                 (remove-duplicates
                  (map uf-find (hash-values sets)))))
           [area_ (set-count area)])
      (* sides area_))))

(define (solve path)
  (let*-values ([(farm) (list->vector
                         (map (compose list->vector string->list)
                              (file->lines path)))])
    (fence-prices farm)))

(solve "input.txt")
