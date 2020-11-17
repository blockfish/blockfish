#lang racket/base
(provide
 ; game state
 tet-game-state?
 make-tet-game-state
 tet-game-state-copy
 tet-game-state-matrix-ref
 tet-game-state-current
 tet-game-state-current/ghost
 tet-game-state-next
 tet-game-state-hold
 tet-move!
 tet-rotate!
 tet-hard-drop!
 tet-hold!
 tet-reset!
 tet-game-state->snapshot
 ; inputs
 input?
 tet-input!
 ; piece state
 piece-state?
 piece-state-color
 piece-state-coords)

(require
 "./mat.rkt"
 "../types.rkt"
 "../brain/snapshot.rkt"
 (prefix-in srs: "../srs.rkt")
 (only-in racket/list shuffle take))

;; ---------------------------------------------------------------------------------------
;; utils

;; rotate-coords : x:nat y:nat extent:nat (between 0 3) -> nat nat
(define (rotate-coords x y w r)
  (define x* (- w x 1))
  (define y* (- w y 1))
  (case r
    [(0) (values x y)]
    [(1) (values y x*)]
    [(2) (values x* y*)]
    [(3) (values y* x)]
    [else (error 'rotate-coords (format "invalid rotation: ~a" r))]))

;; ---------------------------------------------------------------------------------------
;; game state

;; -----
;; color : mino-color
;; row, col : nat
;; rot : (and (>= 0) (< 4))
(struct piece-state
  [color row col rot]
  #:transparent)

;; mino-color -> piece-stat
(define (initial-piece-state mc)
  (define shape (hash-ref srs:4mino-shapes mc))
  (define spawn (shape-data-spawn shape))
  (piece-state mc
               (car spawn)
               (cdr spawn)
               0))

;; piece-state -> [listof [cons row-nat col-nat]]
(define (piece-state-coords ps)
  (define shape (hash-ref srs:4mino-shapes (piece-state-color ps)))
  (for/list ([ij (in-list (shape-data-coords shape))])
    (define-values [j* i*]
      (rotate-coords (cdr ij)
                     (car ij)
                     (shape-data-size shape)
                     (piece-state-rot ps)))
    (cons (+ i* (piece-state-row ps))
          (+ j* (piece-state-col ps)))))

(define (piece-state-intersects? ps mat)
  (for/or ([ij (in-list (piece-state-coords ps))])
    (mat-ref mat (car ij) (cdr ij))))

;; int -> [piece-state -> piece-state]
(define ((piece-state-move dx) ps)
  (struct-copy piece-state ps [col (+ dx (piece-state-col ps))]))

;; int -> [piece-state -> piece-state]
(define ((piece-state-rotate dr) ps)
  (struct-copy piece-state ps [rot (modulo (+ 4 dr (piece-state-rot ps)) 4)]))

;; piece-state -> piece-state
(define (piece-state-drop ps)
  (struct-copy piece-state ps [row (sub1 (piece-state-row ps))]))

;; mat -> [piece-state -> piece-state]
(define ((piece-state-sonic-drop mat) ps)
  (let loop ([ps ps])
    (define ps* (piece-state-drop ps))
    (if (piece-state-intersects? ps* mat)
        ps
        (loop ps*))))

;; -----
;; preview-count : nat
;; next : [listof mino-color]
;; generator : [-> [listof mino-color]]
(struct bag-rand
  [preview-count
   generator
   next]
  #:mutable)

;; -> bag-rand
(define (7bag #:previews [n-previews 5])
  (bag-rand n-previews
            (λ () (shuffle mino-colors))
            '()))

;; bag-rand -> bag-rand
(define (bag-rand-copy bg)
  ;; TODO: make the rng seed explicit
  (struct-copy bag-rand bg))

;; bag-rand -> [listof mino-type]
(define (bag-rand-previews bg)
  (define next (bag-rand-next bg))
  (define cnt (bag-rand-preview-count bg))
  (cond
    [(< (length next) cnt)
     (set-bag-rand-next! bg (append next ((bag-rand-generator bg))))
     (bag-rand-previews bg)]
    [else
     (take next cnt)]))

;; bag-rand -> mino-type
(define (bag-rand-next! bg)
  (begin0 (car (bag-rand-previews bg))
    (set-bag-rand-next! bg (cdr (bag-rand-next bg)))))

;; -----
;; matrix : mat
;; bag : bag-rand
;; hold : (or mino-color #f)
;; current : piece-state
(struct tet-game-state
  [matrix bag hold current]
  #:mutable)

(define (tet-move-by! gs f)
  (define ps* (f (tet-game-state-current gs)))
  (cond
    [(piece-state-intersects? ps* (tet-game-state-matrix gs)) #f]
    [else (set-tet-game-state-current! gs ps*) #t]))

;; tet-game-state int (or 'left 'right) -> boolean
(define (tet-move! gs dir)
  (tet-move-by! gs (piece-state-move (if (eq? dir 'left) -1 +1))))

;; tet-game-state int (or 'cw 'ccw) -> boolean
;; TODO: kick information
(define (tet-rotate! gs dir)
  (tet-move-by! gs (piece-state-rotate  (if (eq? dir 'ccw) -1 +1))))

;; tet-game-state -> void
(define (tet-new-piece! gs)
  (define mc (bag-rand-next! (tet-game-state-bag gs)))
  (define ps (initial-piece-state mc))
  (set-tet-game-state-current! gs ps))

;; tet-game-state -> void
;; TODO: lines cleared information
(define (tet-hard-drop! gs)
  (define mat (tet-game-state-matrix gs))
  (define ps ((piece-state-sonic-drop mat) (tet-game-state-current gs)))
  (mat-blit! mat
             (piece-state-color ps)
             (piece-state-coords ps))
  (mat-clear-filled! mat)
  (tet-new-piece! gs))

;; tet-game-state -> boolean
;; TODO: detect double-hold, return #f
(define (tet-hold! gs)
  (define cur-mc (piece-state-color (tet-game-state-current gs)))
  (define held-mc
    (or (tet-game-state-hold gs)
        (bag-rand-next! (tet-game-state-bag gs))))
  (set-tet-game-state-current! gs (initial-piece-state held-mc))
  (set-tet-game-state-hold! gs cur-mc)
  #t)

;; tet-game-state -> void
(define (tet-reset! gs)
  (set-tet-game-state-matrix! gs (make-mat #:cols srs:cols))
  (set-tet-game-state-bag! gs (7bag))
  (set-tet-game-state-hold! gs #f)
  (mat-add-random-cheese! (tet-game-state-matrix gs) #:rows 8)
  (tet-new-piece! gs))

;; -> tet-game-state
(define (make-tet-game-state)
  (define gs (tet-game-state #f #f #f #f))
  (tet-reset! gs)
  gs)

;; tet-game-state -> tet-game-state
(define (tet-game-state-copy gs)
  (struct-copy tet-game-state gs
               [matrix (mat-copy (tet-game-state-matrix gs))]
               [bag (bag-rand-copy (tet-game-state-bag gs))]))

;; tet-game-state row-nat col-nat -> cell
(define (tet-game-state-matrix-ref gs i j)
  (mat-ref (tet-game-state-matrix gs) i j))

;; tet-game-state -> piece-state
(define (tet-game-state-current/ghost gs)
  ((piece-state-sonic-drop (tet-game-state-matrix gs))
   (tet-game-state-current gs)))

;; tet-game-state -> [listof mino-type]
(define (tet-game-state-next gs)
  (bag-rand-previews (tet-game-state-bag gs)))

;; tet-game-state -> snapshot
(define (tet-game-state->snapshot gs)
  (snapshot (mat->list (tet-game-state-matrix gs))
            (cons (piece-state-color (tet-game-state-current gs))
                  (tet-game-state-next gs))
            (tet-game-state-hold gs)))

;; ---------------------------------------------------------------------------------------
;; inputs

(define (input? x) (memq x '(left right cw ccw hold)))
(define (move-input? x) (memq x '(left right)))
(define (rotate-input? x) (memq x '(cw ccw)))

;; tet-game-state [listof input -> void
(define (tet-input! gs is)
  (for ([i (in-list is)])
    (cond
      [(move-input? i) (tet-move! gs i)]
      [(rotate-input? i) (tet-rotate! gs i)]
      [(eq? i 'hold) (tet-hold! gs)])))
