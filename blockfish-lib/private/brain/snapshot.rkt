#lang racket/base
(provide
 (struct-out snapshot))

;; matrix : (and [listof [vectorof cell-color]] pair)
;; queue : (and [listof mino-color] pair)
;; hold : (or mino-color #f)
(struct snapshot
  [matrix
   queue
   hold]
  #:transparent)
