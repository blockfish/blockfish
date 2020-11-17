#lang racket/base
(provide
 (all-defined-out))

(define mino-colors '(L O J I T S Z))
(define cell-colors `(G H ,@mino-colors))

(define (mino-color? x) (memq x mino-colors))
(define (cell-color? x) (memq x cell-colors))
(define (cell? x) (or (eq? x #f) (cell-color? x)))

;; size : nat
;; spawn : (cons row-nat col-nat)
;; coords : [listof (cons row-nat col-nat)]
(struct shape-data [size coords spawn] #:transparent)
