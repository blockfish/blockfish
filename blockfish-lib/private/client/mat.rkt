#lang racket/base
(require racket/contract/base)
(provide
 mat?
 (contract-out
  [make-mat (-> #:cols exact-nonnegative-integer?
                mat?)]
  [mat-copy (-> mat? mat?)]
  [mat-ref (-> mat?
               exact-integer?
               exact-integer?
               any)]
  [mat-blit! (-> mat?
                 any/c
                 (listof (cons/c exact-integer? exact-integer?))
                 any)]
  [mat-clear-filled! (-> mat?
                         any)]
  [mat-add-random-cheese! (-> mat?
                              #:rows exact-nonnegative-integer?
                              any)]))

(require
 racket/vector)

(module+ test
  (require rackunit))

;; ---------------------------------------------------------------------------------------

;; num-cols : nat
;; rows : rows
;; --
;; rows = [vectorof [vector cell #:length num-cols]]
(struct mat [num-cols rows] #:mutable)

;; -> mat
(define (make-mat #:cols num-cols)
  (mat num-cols #()))

(define (mat-copy m)
  (struct-copy mat m
               [rows (vector-map vector-copy (mat-rows m))]))

;; mat [rows -> rows] -> mat
(define (modify-mat-rows! m f)
  (set-mat-rows! m (f (mat-rows m))))

;; mat -> nat
(define (mat-nonempty-lines m)
  (vector-length (mat-rows m)))

;; mat row-nat col-nat -> cell
(define (mat-ref m i j)
  (if (or (< i 0)
          (< j 0)
          (>= j (mat-num-cols m)))
      'H
      (and (< i (mat-nonempty-lines m))
           (vector-ref (vector-ref (mat-rows m) i) j))))

(define (row-holes r)
  (for/sum ([x (in-vector r)])
    (if x 0 1)))

;; mat -> void
(define (mat-clear-filled! m)
  (define (prune rows)
    (for*/vector ([row (in-vector rows)]
                  #:when (> (row-holes row) 0))
      row))
  (modify-mat-rows! m prune))

;; mat mino-color [listof (cons row-nat col-nat)] -> void
(define (mat-blit! m color coords)
  (define n-cols (mat-num-cols m))
  (define n-rows (mat-nonempty-lines m))
  (define n-rows* (for/fold ([n n-rows])
                            ([ij (in-list coords)])
                    (max n (add1 (car ij)))))
  (define (insert-new-rows rows)
    (for/vector ([i (in-range n-rows*)])
      (if (>= i n-rows)
          (make-vector n-cols #f)
          (vector-ref rows i))))
  (modify-mat-rows! m insert-new-rows)
  (for ([ij (in-list coords)])
    (vector-set! (vector-ref (mat-rows m) (car ij))
                 (cdr ij)
                 color)))

;; =============================================

(module+ test
  (let ([m (make-mat #:cols 3)])
    (mat-blit! m 'J '([0 . 0] [0 . 1] [0 . 2] [1 . 0]))
    (check-equal? (mat-rows m)
                  (vector #(J  J  J)
                          #(J  #f #f)))

    (mat-blit! m 'I '([1 . 2] [2 . 2] [3 . 2] [4 . 2]))
    (check-equal? (mat-rows m)
                  (vector #(J  J  J)
                          #(J  #f I)
                          #(#f #f I)
                          #(#f #f I)
                          #(#f #f I)))
    (mat-clear-filled! m)
    (check-equal? (mat-rows m)
                  (vector #(J  #f I)
                          #(#f #f I)
                          #(#f #f I)
                          #(#f #f I)))

    (mat-blit! m 'Z '([0 . 1] [1 . 0] [1 . 1] [2 . 0]))
    (check-equal? (mat-rows m)
                  (vector #(J  Z  I)
                          #(Z  Z  I)
                          #(Z  #f I)
                          #(#f #f I)))
    (mat-clear-filled! m)
    (check-equal? (mat-rows m)
                  (vector #(Z  #f I)
                          #(#f #f I))))

  (let ([m (make-mat #:cols 3)])
    (mat-blit! m 'J '([0 . 0] [0 . 1] [0 . 2] [1 . 0]))
    (check-equal? (mat-rows m)
                  (vector #(J  J  J)
                          #(J  #f #f)))

    (define m* (mat-copy m))

    (mat-blit! m 'I '([1 . 2] [2 . 2] [3 . 2] [4 . 2]))
    (check-equal? (mat-rows m)
                  (vector #(J  J  J)
                          #(J  #f I)
                          #(#f #f I)
                          #(#f #f I)
                          #(#f #f I)))
    (check-equal? (mat-rows m*)
                  (vector #(J  J  J)
                          #(J  #f #f)))))


;; ---------------------------------------------------------------------------------------
;; cheese

(define (mat-add-random-cheese! m #:rows cnt)
  (define cheese (cheese-row-generator #:cols (mat-num-cols m)))
  (define (insert rows)
    (vector-append (build-vector cnt (λ (_i) (cheese)))
                   rows))
  (modify-mat-rows! m insert))

(define (cheese-row-generator #:cols n-cols)
  (define j0 #f)
  (λ ()
    (define j (if j0
                  (modulo (+ j0 1 (random (sub1 n-cols))) n-cols)
                  (random n-cols)))
    (set! j0 j)
    (cheese-row j #:cols n-cols)))

(define (cheese-row j #:cols n-cols)
  (build-vector n-cols (λ (j*) (and (not (= j j*)) 'G))))

;; ==========================================

(module+ test
  (let ([m (make-mat #:cols 10)])
    (check-equal? (mat-nonempty-lines m) 0)

    (mat-add-random-cheese! m #:rows 3)
    (check-equal? (mat-nonempty-lines m) 3)

    (mat-add-random-cheese! m #:rows 4)
    (check-equal? (mat-nonempty-lines m) 7)
    (check-equal? (vector-map row-holes (mat-rows m))
                  (make-vector 7 1))))
