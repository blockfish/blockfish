#lang racket/base
(provide
 ai-suggest)

(require
 "./snapshot.rkt"
 (only-in racket/list make-list))

;; snapshot -> [listof [listof input?]]
;; TODO: input game state repr
(define (ai-suggest ss)
  (sleep 1/2)
  (for/list ([mdir (in-list '(left right))])
    (make-list 6 mdir)))

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
    (define w 300)
    (define h 300)
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
    (define sugs (ai-suggest (tet-game-state->snapshot gs)))
    (suggestions->snips gs sugs))

  (ai-suggest->snips (make-tet-game-state))

  )
