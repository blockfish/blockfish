#lang racket/base
(provide
 (struct-out theme)
 default-theme
 tris-view%
 paint-view)

(require
 racket/class
 racket/gui/base
 (only-in racket/math exact-floor))

;; ---------------------------------------------------------------------------------------
;; theme

(struct theme [font bg border text cells] #:transparent)

(define default-theme
  (theme
   (make-object font% 24.0 'system)
   (make-object color% #x00 #x00 #x00) #| #000 |#
   (make-object color% #x22 #x22 #x22) #| #222 |#
   (make-object color% #xdd #xdd #xdd) #| #ddd |#
   (hasheq 'L (make-object color% #xff #x88 #x00) #| #f80 |#
           'O (make-object color% #xff #xff #x00) #| #ff0 |#
           'J (make-object color% #x00 #x00 #xff) #| #00f |#
           'I (make-object color% #x00 #xff #xff) #| #0ff |#
           'T (make-object color% #x88 #x00 #xff) #| #80f |#
           'S (make-object color% #x00 #xff #x00) #| #0f0 |#
           'Z (make-object color% #xff #x00 #x00) #| #f00 |#
           'G (make-object color% #xaa #xaa #xaa) #| #aaa |#
           'H (make-object color% #x66 #x66 #x66) #| #666 |# )))

;; ---------------------------------------------------------------------------------------
;; rendering

(define tris-view%
  (class object%
    (super-new)

    ;; -> theme
    (define/public (get-theme) default-theme)

    ;; mino-type -> [sequenceof (cons row-nat col-nat)]
    (define/public (get-shape-coords _) '())

    ;; -> nat
    (define/public (get-visible-rows) 1)
    (define/public (get-visible-cols) 1)

    ;; -> [hash mino-type => [sequenceof (cons row-nat col-nat)]]
    (define/public (get-matrix-coords) (hasheq))

    ;; -> mino-color
    (define/public (get-piece-color) 'T)

    ;; [sequenceof (cons row-nat col-nat)]
    (define/public (get-piece-coords) '())
    (define/public (get-ghost-coords) '())

    ;; -> (or #f mino-color)
    (define/public (get-suggest-color) #f)
    ;; [sequenceof (cons row-nat col-nat)]
    (define/public (get-suggest-coords) '())

    ;; -> (or #f mino-type)
    (define/public (get-hold) #f)

    ;; -> [sequenceof mino-type]
    (define/public (get-previews) '())

    ;; -> [sequenceof string]
    (define/public (get-stats) '())))

;; dc<%> view% -> void
(define (paint-view view dc)
  (define-values [dc-w dc-h] (send dc get-size))
  (define visible-rows (send view get-visible-rows))
  (define visible-cols (send view get-visible-cols))
  (define thm (send view get-theme))

  ; standard 1mino size
  (define sz
    (exact-floor (min (/ dc-h visible-rows)
                      (/ dc-w (+ visible-cols 8)))))

  (define (cell-color cl)
    (hash-ref (theme-cells thm) cl))

  ;; ----
  ;; clear background

  (send dc set-background (theme-bg thm))
  (send dc clear)

  ;; ----
  ;; draw board

  (define (set-solid mc)
    (send dc set-pen "black" 0 'transparent)
    (send dc set-brush (cell-color mc) 'solid))

  (define (set-ghost mc)
    (send dc set-pen (cell-color mc) 2 'solid)
    (send dc set-brush "black" 'transparent))

  (define (set-suggest mc)
    (send dc set-pen (cell-color mc) 1 'dot)
    (send dc set-brush "black" 'transparent))

  (define mat-w (* sz visible-cols))
  (define mat-h (* sz visible-rows))
  (define mat-x0 (exact-floor (/ (- dc-w mat-w) 2)))
  (define mat-y0 dc-h)

  (define (render-coords coords)
    (for ([ij coords])
      (send dc draw-rectangle
            (+ mat-x0 (* (cdr ij) sz))
            (- mat-y0 (* (car ij) sz) sz)
            sz
            sz)))

  ; matrix
  (for ([(mc coords) (in-hash (send view get-matrix-coords))])
    (set-solid mc)
    (render-coords coords))

  ; suggestion
  (let ([mc (send view get-suggest-color)])
    (when mc
      (set-suggest mc)
      (render-coords (send view get-suggest-coords))))

  ; ghost
  (set-ghost (send view get-piece-color))
  (render-coords (send view get-ghost-coords))

  ; current piece
  (set-solid (send view get-piece-color))
  (render-coords (send view get-piece-coords))

  ; border
  (send dc set-brush "black" 'transparent)
  (send dc set-pen (theme-border thm) 2 'solid)
  (send dc draw-rectangle mat-x0 (- mat-y0 mat-h) mat-w mat-h)

  ;; ----
  ;; draw hold & next

  (define hold-x0 0)
  (define hold-y0 0)
  (define next-x0 (- dc-w (* sz 4)))
  (define next-y0 0)

  (define (render-polymino mc x y)
    (send dc set-pen "black" 0 'transparent)
    (send dc set-brush (cell-color mc) 'solid)
    (define y* (+ y (* sz 3)))
    (for ([ij (send view get-shape-coords mc)])
      (send dc draw-rectangle
            (+ x  (* (cdr ij) sz))
            (- y* (* (car ij) sz) sz)
            sz
            sz)))

  ; hold
  (let ([hold (send view get-hold)])
    (when hold
      (render-polymino hold hold-x0 hold-y0)))

  ; next
  (for ([mc (send view get-previews)]
        [i (in-naturals)])
    (define next-y (+ next-y0 (* sz 3 i)))
    (render-polymino mc next-x0 next-y))

  ;; ----
  ;; draw stats

  (define stats-x0 0)
  (define stats-y0 dc-h)

  (send dc set-font (theme-font thm))
  (define-values [_tw text-h _bh _vs] (send dc get-text-extent ""))

  (send dc set-text-mode 'transparent)
  (send dc set-text-foreground (theme-text thm))
  (for ([stat (send view get-stats)]
        [i (in-naturals)])
    (send dc draw-text
          stat
          stats-x0
          (- stats-y0 text-h (* text-h i)))))

(module+ test
  ;; view% -> snip%
  (define (view->snip view
                      #:width w
                      #:height h)
    (define bm (make-object bitmap% w h))
    (paint-view view (make-object bitmap-dc% bm))
    (make-object image-snip% bm))

  (view->snip
   #:width 240
   #:height 200
   (new (class* tris-view% ()
          (super-new)
          (define/override (get-visible-rows) 8)
          (define/override (get-visible-cols) 5)

          (define/override (get-matrix-coords)
            #hash([L . [(0 . 2) (0 . 3) (0 . 4) (1 . 4)]]
                  [Z . [(0 . 1) (1 . 1) (1 . 2) (2 . 2)]]
                  [I . [(1 . 3) (2 . 3) (3 . 3) (4 . 3)]]))

          (define/override (get-piece-color) 'S)
          (define/override (get-ghost-coords) '[(4 . 1) (4 . 2) (5 . 2) (5 . 3)])

          (define/override (get-stats)
            '("APP: 0"
              "APM: 0"
              "PPS: 0.3"))))))
