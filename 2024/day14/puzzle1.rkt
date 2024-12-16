#lang racket

(require math/number-theory)

(define (safety-factor pos)
  (let ([xs (map first pos)]
        [ys (map second pos)]
        [xm (quotient width 2)]
        [ym (quotient height 2)])
    (* (count (λ (x y) (and (x . < . xm) (y . < . ym))) xs ys)
       (count (λ (x y) (and (x . > . xm) (y . < . ym))) xs ys)
       (count (λ (x y) (and (x . < . xm) (y . > . ym))) xs ys)
       (count (λ (x y) (and (x . > . xm) (y . > . ym))) xs ys))))

(define (simulate robots [seconds 100])
  (map
   (match-λ
    [`((,p1 ,p2) (,v1 ,v2))
     (list (with-modulus width
             (mod+ p1 (mod* seconds v1)))
           (with-modulus height
             (mod+ p2 (mod* seconds v2))))])
   robots))


(define (solve path)
  (let*-values ([(input) (file->lines path)])
    (safety-factor
     (simulate (for/list ([spec input])
                 (for/list ([regex '(#px"p=([-0-9]*),([-0-9]*)"
                                     #px"v=([-0-9]*),([-0-9]*)")])
                   (map string->number
                        (first (regexp-match* regex spec
                                              #:match-select rest)))))))))


(define width 101)
(define height 103)

(solve "input.txt")
