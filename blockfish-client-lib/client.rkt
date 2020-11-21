#lang racket/base
(require
 "./engine.rkt"
 "./private/client/game.rkt"
 "./private/client/game-view.rkt"
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
    (inherit refresh get-dc)
    (super-new)

    (define view
      (new game-view%
           [game-state gs]
           [tris-theme thm]))

    (define suggest-id #f)
    (define suggest-game-state #f)

    (define (start-suggest)
      (set! suggest-id (gensym))
      (set! suggest-game-state (tet-game-state-copy gs))
      (send view set-suggestions '())
      (refresh)
      (finish-suggest suggest-id))

    (define ((finish-suggest this-suggest-id) suggestions)
      (when (eq? suggest-id this-suggest-id)
        (send view set-suggestions
              (for/list ([is (in-list suggestions)])
                (define gs (tet-game-state-copy suggest-game-state))
                (tet-input! gs is)
                gs))
        (refresh)))

    (define (do-suggest)
      (define finish-proc (start-suggest))
      (define snapshot (tet-game-state->snapshot gs))
      (thread (λ ()
                (define result (run-engine snapshot))
                (queue-callback (λ () (finish-proc result))))))

    (do-suggest)

    (define/override (on-paint)
      (paint-view view (get-dc)))

    (define/override (on-event e)
      (case (send e get-event-type)
        [(left-down)
         (tet-reset! gs)
         (do-suggest)
         (refresh)]))

    (define/override (on-char e)
      (case (send e get-key-code)
        [(left)
         (when (tet-move! gs 'left)
           (refresh))]
        [(right)
         (when (tet-move! gs 'right)
           (refresh))]
        [(#\z)
         (when (tet-rotate! gs 'ccw)
           (refresh))]
        [(#\x)
         (when (tet-rotate! gs 'cw)
           (refresh))]
        [(#\space)
         (tet-hard-drop! gs)
         (do-suggest)
         (refresh)]
        [(#\s)
         (send view switch-suggestion +1)
         (refresh)]
        [(#\a)
         (send view switch-suggestion -1)
         (refresh)]
        [(shift)
         (when (tet-hold! gs)
           (refresh))]))))

(module+ main
  (send (new blockfish-frame%
             [game-state (make-tet-game-state)]
             [tris-theme default-theme])
        show #t))
