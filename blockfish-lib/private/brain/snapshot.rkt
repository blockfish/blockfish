#lang racket/base
(provide
 (struct-out snapshot)
 snapshot-num-cols)

;; matrix : (and [listof [vectorof cell-color]] pair)
;; queue : (and [listof mino-color] pair)
;; hold : (or mino-color #f)
(struct snapshot
  [matrix
   queue
   hold]
  #:transparent)

(define (snapshot-num-cols ss)
  (vector-length (car (snapshot-matrix ss))))
