#lang racket

(require math/matrix)

(define (prize a b p)
  ;; NOTE: zeros are not handled since there are none in the input
  (match-let* ([(list a1 a2) a]
               [(list b1 b2) b]
               [(list p1 p2) p]
               [M (matrix [[a1 b1]
                           [a2 b2]])]
               [b (col-matrix [p1 p2])])
    (cond
      ;; unique solution
      [(matrix-invertible? M)
       (let ([x (matrix->list (matrix-solve M b))])
         (if (andmap (conjoin positive? integer?) x)
             (+ (* 3 (first x))
                (second x))
             0))]
      ;; linearly dependent
      [(let ([factor (/ p1 p2)])
         (and (= (* factor a2) a1)
              (= (* factor b2) b1))
         (cond
           [(and (= (modulo p1 a1) 0)
                 (= (modulo p1 b1) 0))
            (min (* 3 (quotient p1 a1))
                 (quotient p1 b1))]
           [(= (modulo p1 a1) 0)
            (* 3 (quotient p1 a1))]
           [(= (modulo p1 b1) 0)
            (quotient p1 b1)]
           [else 0]))] 
      [else 0])))

(define (solve path)
  (let*-values ([(input) (string-split (file->string path) "\n\n")])
    (for/sum ([spec input])
      (apply prize
             (for*/list ([regex '(#px"Button A: X\\+([0-9]*), Y\\+([0-9]*)"
                                  #px"Button B: X\\+([0-9]*), Y\\+([0-9]*)"
                                  #px"Prize: X=([0-9]*), Y=([0-9]*)")])
               (map string->number
                    (first (regexp-match* regex spec
                                          #:match-select rest))))))))

(solve "input.txt")
