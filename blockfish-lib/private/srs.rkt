#lang racket/base
(provide
 (all-defined-out))

(require
 racket/function)

(define rows 22)
(define cols 10)

(define mino-colors '(L O J I T S Z))
(define cell-colors `(G H ,@mino-colors))

(define (mino-color? x) (memq x mino-colors))
(define (cell-color? x) (memq x cell-colors))
(define (cell? x) (or (eq? x #f) (cell-color? x)))

;; size : nat
;; spawn : (cons row-nat col-nat)
;; coords : [listof (cons row-nat col-nat)]
(struct shape-data [size coords spawn] #:transparent)

(define 4mino-shapes
  (hasheq 'J (shape-data 3 '[(1 . 0)  (1 . 1)  (1 . 2)  (2 . 0)] '(18 . 3))
          'T (shape-data 3 '[(1 . 0)  (1 . 1)  (1 . 2)  (2 . 1)] '(18 . 3))
          'L (shape-data 3 '[(1 . 0)  (1 . 1)  (1 . 2)  (2 . 2)] '(18 . 3))
          'O (shape-data 4 '[(1 . 1)  (2 . 1)  (1 . 2)  (2 . 2)] '(18 . 3))
          'S (shape-data 3 '[(1 . 0)  (1 . 1)  (2 . 1)  (2 . 2)] '(18 . 3))
          'Z (shape-data 3 '[(2 . 0)  (1 . 1)  (2 . 1)  (1 . 2)] '(18 . 3))
          'I (shape-data 4 '[(2 . 0)  (2 . 1)  (2 . 2)  (2 . 3)] '(17 . 3))))
