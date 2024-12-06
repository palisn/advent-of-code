#lang racket

(require math/base)

;; Create adjacency lists from list of rules
(define (rules->adj rules)
  (foldl (λ (rule acc)
           (match-let ([(list a b) rule])
             (hash-set acc a (set-add (hash-ref acc a) b))))
         (make-immutable-hash (map (curryr cons (set))
                         (map first rules)))
         rules))

(define (fix-print-queue rules updates)
  (define (middle xs) (list-ref xs (quotient (length xs) 2)))
  ;; Perform topological sorting
  (define (fix xs)
    (rest
     (let* ([adj (rules->adj rules)]
            [adj (hash-set adj 0 (hash-keys adj))]
            [state (make-hash (map (curryr cons 'todo) (cons 0 xs)))])
       (let dfs ([v 0])
         (cond
           [(member v (hash-keys state))
            (hash-set! state v 'active)
            (begin0
                (cons
                 v
                 (for/fold ([acc '()])
                           ([u (hash-ref adj v (set))]
                            #:unless (eq? (hash-ref state u 0) 'done))
                   (when (eq? (hash-ref state u 0) 'active)
                     (error 'cyclic-graph))
                   (append (dfs u) acc)))
              (hash-set! state v 'done))]
           [else '()])))))
  
  (define (fn x acc)
    (let* ([correct? (first acc)]
           [pages (second acc)]
           [pages-after-x
            (map second (filter (compose (curry = x) first) rules))])
      (list (and correct? (not (set-member? pages x)))
            (set-union pages (list->set pages-after-x)))))
  
  (sum (map (compose middle fix)
            (filter (negate (compose first (curry foldr fn (list #t (set)))))
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
    (fix-print-queue ordering-rules updates)))

(solve "input.txt")
