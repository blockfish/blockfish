#lang racket/base
(provide
 game-view%)

(require
 "./game.rkt"
 "./render.rkt"
 "../types.rkt"
 (prefix-in srs: "../srs.rkt")
 racket/class
 racket/gui)

(define game-view%
  (class tris-view%
    (init-field [(gs game-state)] [(thm tris-theme)])
    (super-new)
    (define vis-rows 20)
    (define vis-cols 10)

    (define/override (get-theme) thm)
    (define/override (get-visible-rows) vis-rows)
    (define/override (get-visible-cols) vis-cols)

    (define/override (get-shape-coords mc)
      (shape-data-coords (hash-ref srs:4mino-shapes mc)))

    (define/override (get-matrix-coords)
      (define m (make-hasheq))
      (for* ([i (in-range vis-rows)]
             [j (in-range vis-cols)])
        (define cl (tet-game-state-matrix-ref gs i j))
        (when cl
          (hash-update! m cl
                        (λ (ijs) (cons (cons i j) ijs))
                        '())))
      m)

    (define/override (get-piece-color)
      (piece-state-color (tet-game-state-current gs)))

    (define/override (get-piece-coords)
      (piece-state-coords (tet-game-state-current gs)))

    (define/override (get-ghost-coords)
      (piece-state-coords (tet-game-state-current/ghost gs)))

    (define/override (get-hold)
      (tet-game-state-hold gs))

    (define/override (get-previews)
      (tet-game-state-next gs))

    (define suggestions #())
    (define sugidx 0)
    (define (no-suggestions?) (zero? (vector-length suggestions)))

    (define/public (set-suggestions lst)
      (set! suggestions (for/vector #:length (length lst)
                            ([gs* (in-list lst)])
                          (tet-game-state-current/ghost gs*)))
      (set! sugidx 0))

    (define/public (switch-suggestion delta)
      (unless (no-suggestions?)
        (define len (vector-length suggestions))
        (set! sugidx (modulo (+ sugidx len delta) len))))

    (define/override (get-suggest-color)
      (if (no-suggestions?) #f
          (piece-state-color (vector-ref suggestions sugidx))))

    (define/override (get-suggest-coords)
      (piece-state-coords (vector-ref suggestions sugidx)))

    (define/override (get-stats)
      (if (no-suggestions?)
          (list "Computing...")
          (list (format "Engine: (~a/~a)"
                        (add1 sugidx)
                        (vector-length suggestions)))))))
