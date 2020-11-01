#lang racket/base
(require
 "./private/client/game.rkt"
 "./private/client/render.rkt"
 "./private/srs.rkt"
 racket/class
 racket/gui)

(define blockfish-frame%
  (class frame%
    (init-field game-state
                tris-theme)

    (super-new [label "Blockfish"]
               [width 600]
               [height 700])

    (new blockfish-canvas%
         [parent this]
         [game-state game-state]
         [tris-theme tris-theme])))

(define blockfish-canvas%
  (class canvas%
    (init-field [(gs game-state)] [(thm tris-theme)])
    (super-new)

    (define view
      (new blockfish-view%
           [game-state gs]
           [tris-theme thm]))

    (define (suggest)
      (send view set-suggestions
            (for*/list ([mdir (in-list '(left right))]
                        [rdir (in-list '(#f cw ccw))])
              (define gs* (tet-game-state-copy gs))
              (when rdir (tet-rotate! gs* rdir))
              (let das () (when (tet-move! gs* mdir) (das)))
              gs*)))

    (suggest)

    (define/override (on-paint)
      (paint-view view (send this get-dc)))

    (define/override (on-event e)
      (case (send e get-event-type)
        [(left-down)
         (tet-reset! gs)
         (suggest)
         (send this refresh)]))

    (define/override (on-char e)
      (case (send e get-key-code)
        [(left)
         (when (tet-move! gs 'left)
           (send this refresh))]
        [(right)
         (when (tet-move! gs 'right)
           (send this refresh))]
        [(#\z)
         (when (tet-rotate! gs 'ccw)
           (send this refresh))]
        [(#\x)
         (when (tet-rotate! gs 'cw)
           (send this refresh))]
        [(#\space)
         (tet-hard-drop! gs)
         (suggest)
         (send this refresh)]
        [(#\s)
         (send view switch-suggestion)
         (send this refresh)]
        [(shift)
         (when (tet-hold! gs)
           (suggest)
           (send this refresh))]))))

(define blockfish-view%
  (class tris-view%
    (init-field [(gs game-state)] [(thm tris-theme)])
    (super-new)
    (define vis-rows 20)
    (define vis-cols 10)

    (define/override (get-theme) thm)
    (define/override (get-visible-rows) vis-rows)
    (define/override (get-visible-cols) vis-cols)

    (define/override (get-shape-coords mc)
      (shape-data-coords (hash-ref 4mino-shapes mc)))

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

    (define/public (switch-suggestion)
      (unless (no-suggestions?)
        (set! sugidx (modulo (add1 sugidx) (vector-length suggestions)))))

    (define/override (get-suggest-color)
      (if (no-suggestions?) #f
          (piece-state-color (vector-ref suggestions sugidx))))

    (define/override (get-suggest-coords)
      (piece-state-coords (vector-ref suggestions sugidx)))

    (define/override (get-stats)
      (if (no-suggestions?)
          '()
          (list (format "Engine: (~a/~a)"
                        (add1 sugidx)
                        (vector-length suggestions)))))))

(module+ main
  (send (new blockfish-frame%
             [game-state (make-tet-game-state)]
             [tris-theme default-theme])
        show #t))
