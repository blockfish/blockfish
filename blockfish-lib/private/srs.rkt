#lang racket/base
(provide
 (all-defined-out))

(require
 racket/function
 "./types.rkt")

(define rows 22)
(define cols 10)

(define 4mino-shapes
  (hasheq 'J (shape-data 3 '[(1 . 0)  (1 . 1)  (1 . 2)  (2 . 0)] '(18 . 3))
          'T (shape-data 3 '[(1 . 0)  (1 . 1)  (1 . 2)  (2 . 1)] '(18 . 3))
          'L (shape-data 3 '[(1 . 0)  (1 . 1)  (1 . 2)  (2 . 2)] '(18 . 3))
          'O (shape-data 4 '[(1 . 1)  (2 . 1)  (1 . 2)  (2 . 2)] '(18 . 3))
          'S (shape-data 3 '[(1 . 0)  (1 . 1)  (2 . 1)  (2 . 2)] '(18 . 3))
          'Z (shape-data 3 '[(2 . 0)  (1 . 1)  (2 . 1)  (1 . 2)] '(18 . 3))
          'I (shape-data 4 '[(2 . 0)  (2 . 1)  (2 . 2)  (2 . 3)] '(17 . 3))))
