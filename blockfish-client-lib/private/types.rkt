#lang racket/base
(provide
 (all-defined-out))

(define mino-colors '(L O J I T S Z))
(define cell-colors `(G H ,@mino-colors))

(define (mino-color? x) (memq x mino-colors))
(define (cell-color? x) (memq x cell-colors))
(define (cell? x) (or (eq? x #f) (cell-color? x)))

(define (input? x) (memq x '(left right cw ccw hold)))
(define (move-input? x) (memq x '(left right)))
(define (rotate-input? x) (memq x '(cw ccw)))

;; size : nat
;; spawn : (cons row-nat col-nat)
;; coords : [listof (cons row-nat col-nat)]
(struct shape-data [size coords spawn] #:transparent)

;; matrix : (and [listof [vectorof cell-color]] pair)
;; queue : (and [listof mino-color] pair)
;; hold : (or mino-color #f)
(struct snapshot [matrix queue hold] #:transparent)

;; inputs : [listof input]
;; score : real
(struct suggestion [inputs score] #:transparent)
