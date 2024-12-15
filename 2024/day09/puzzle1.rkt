#lang racket

(require point-free)

(define (range-sum start count)
  (let ([start (sub1 start)])
    (- (/ (* (+ start count) (add1 (+ start count))) 2)
       (/ (* start (add1 start)) 2))))

(define (compacted-checksum disk_)
  (define disk-length (length disk_))
  (define disk (for/hash ([i (in-naturals 0)] [n disk_])
                  (values i n)))
  (let rec ([idx 0]
            [block 0]
            [curr-file (* 2 (quotient (sub1 disk-length) 2))]
            [disk disk])
    (cond
      [(or (not (hash-has-key? disk idx))
           (not (hash-has-key? disk curr-file)))
       0]
      [(odd? idx)
       (let* ([file-idx (/ curr-file 2)]
              [available (hash-ref disk idx)]
              [transfer (hash-ref disk curr-file)]
              [size (min transfer available)]
              [next-file? (= size transfer)])
         (+ (* file-idx (range-sum block size))
            (rec (if next-file? idx (add1 idx))
                 (+ block size)
                 (if next-file? (- curr-file 2) curr-file)
                 (~> disk
                     (curryr hash-set curr-file (- transfer size))
                     (curryr hash-set idx (- available size))))))]
      [(even? idx)
       (let ([file-idx (/ idx 2)]
             [size (hash-ref disk idx)])
         (+ (* file-idx (range-sum block size))
            (rec (add1 idx)
                 (+ block size)
                 curr-file
                 (hash-set disk idx 0))))])))

(define (solve path)
  (let*-values ([(disk) (map (compose string->number string)
                             (string->list (string-trim (file->string path))))])
    (compacted-checksum disk)))

(solve "input.txt")
