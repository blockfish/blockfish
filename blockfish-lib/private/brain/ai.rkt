#lang racket/base
(provide
 ai)

(require
 "./snapshot.rkt"
 "./norm-shape.rkt"
 "./mat.rkt"
 racket/list)

;; shape : norm-shape
;; col : col-nat
(struct placement
  [shape col] #:transparent)

;; lower score is better
(define (placement-score mat pm)
  (define ns (placement-shape pm))
  (define j (placement-col pm))
  (define i (sonic-drop ns mat j))
  (let ([mat (mat-clear-lines (blit-shape mat ns i j))])
    (- (* (mat-num-cols mat)
          (mat-num-rows mat))
       (mat-num-cells mat))))

(define (placement->string pm)
  (format "~a(~a), column ~a"
          (norm-shape-color (placement-shape pm))
          (norm-shape-rot-string (placement-shape pm))
          (placement-col pm)))

(define (placement-inputs pm)
  (inputs/column (placement-shape pm)
                 (placement-col pm)))

;; snapshot -> [listof [listof input]]
(define (ai ss)
  (define mat (make-mat (snapshot-matrix ss)))
  (define color (car (snapshot-queue ss)))
  (define norm-shapes (hash-ref srs-norm-shapes/color color))
  (define placement+scores
    (for*/list ([ns (in-list norm-shapes)]
                [j (in-range (add1 (- (mat-num-cols mat)
                                      (norm-shape-num-cols ns))))])
      (define pm (placement ns j))
      (cons (placement-score mat pm)
            pm)))
  (printf "--------\n")
  (for/list ([idx (in-range 99)]
             [s+pm (in-list (sort placement+scores < #:key car))])
    (printf "(~a) ~a: score = ~a\n"
            (add1 idx)
            (placement->string (cdr s+pm))
            (car s+pm))
    (placement-inputs (cdr s+pm))))

;; =======================================================================================

(module+ test
  (require
   "../client/game.rkt"
   "../client/game-view.rkt"
   "../client/render.rkt"
   racket/class
   racket/gui)

  (define suggested-game-view%
    (class game-view%
      (super-new)
      (inherit get-piece-color)
      (define/override (get-piece-coords) '())
      (define/override (get-ghost-coords) '())
      (define/override (get-suggest-color) (get-piece-color))
      (define/override (get-suggest-coords) (super get-ghost-coords))
      (define/override (get-stats) '())))

  (define (game-state->snip gs)
    (define w 200)
    (define h 200)
    (define bm (make-object bitmap% w h))
    (paint-view (new suggested-game-view%
                     [game-state gs]
                     [tris-theme default-theme])
                (make-object bitmap-dc% bm))
    (make-object image-snip% bm))

  (define (suggestions->snips gs suggestions)
    (for/list ([inputs (in-list suggestions)])
      (define gs* (tet-game-state-copy gs))
      (tet-input! gs* inputs)
      (game-state->snip gs*)))

  (define (ai-suggest->snips gs)
    (define sugs (ai (tet-game-state->snapshot gs)))
    (suggestions->snips gs sugs))

  (ai-suggest->snips (make-tet-game-state)))
