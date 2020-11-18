#lang racket/base
(provide
 norm-shape?
 make-norm-shape
 norm-shape-num-rows norm-shape-num-cols norm-shape-color
 norm-shape-hit-ref
 norm-shape-floor
 norm-shape-rot-string
 sonic-drop
 blit-shape
 ;; ---
 inputs/column
 ;; ---
 srs-norm-shapes
 srs-norm-shapes/color
 srs-norm-shapes/color+rot)

(require
 "./mat.rkt"
 racket/list
 racket/string)

(module+ test
  (require rackunit racket/function))

;; ---------------------------------------------------------------------------------------
;; normalized shape representation

(define (rot-info? r)
  ; (cons rot initial-col)
  (and (pair? r)
       (exact-integer? (car r))
       (exact-integer? (cdr r))
       (<= 0 (car r) 3)))

;; color : mino-color
;; rots : [listof rot-info]
;; num-rows, num-cols : nat
;; hit-matrix : (vectorof boolean)
(struct norm-shape
  [color rot-infos num-rows num-cols hit-matrix])

;; mino-color nat (vectorof boolean) [listof rot-info] -> norm-shape
(define (make-norm-shape color n-cols hm rot)
  (define n-rows (/ (vector-length hm) n-cols))
  (norm-shape color rot n-rows n-cols hm))

;; norm-shape int int -> boolean
(define (norm-shape-hit-ref ns i j)
  (define n-rows (norm-shape-num-rows ns))
  (define n-cols (norm-shape-num-cols ns))
  (and (>= i 0) (< i n-rows)
       (>= j 0) (< j n-cols)
       (vector-ref (norm-shape-hit-matrix ns)
                   (+ (* i n-cols) j))))

;; norm-shape col-nat -> row-nat
(define (norm-shape-floor ns j)
  (for/or ([i (in-naturals)])
    (and (norm-shape-hit-ref ns i j) i)))

;; norm-shape row-nat col-nat -> [sequenceof (cons row-nat col-nat)]
(define (in-norm-shape-coords ns [i0 0] [j0 0])
  (make-do-sequence
   (coords-generator i0
                     j0
                     (norm-shape-num-cols ns)
                     (norm-shape-hit-matrix ns))))

(define ((coords-generator i0 j0 num-cols hit-matrix))
  (define (coord pos)
    (define-values [i j] (quotient/remainder pos num-cols))
    (cons (+ i i0) (+ j j0)))
  (define (next pos)
    (define pos* (add1 pos))
    (if (or (>= pos* (vector-length hit-matrix))
            (vector-ref hit-matrix pos*))
        pos*
        (next pos*)))
  (values coord
          next
          (next -1)
          (λ (pos) (< pos (vector-length hit-matrix)))
          #f
          #f))

(define (norm-shape-rot-string ns)
  (string-join (for/list ([ri (in-list (norm-shape-rot-infos ns))])
                 (case (car ri)
                   [(0) "ini"]
                   [(1) "cw"]
                   [(2) "180"]
                   [(3) "ccw"]))
               "/"))

;; ==========================================

(module+ test
  (define s0 (hash-ref srs-norm-shapes/color+rot '(S . 0)))
  (define s1 (hash-ref srs-norm-shapes/color+rot '(S . 1)))
  (define z0 (hash-ref srs-norm-shapes/color+rot '(Z . 0)))
  (define z1 (hash-ref srs-norm-shapes/color+rot '(Z . 1)))
  (define t0 (hash-ref srs-norm-shapes/color+rot '(T . 0)))
  (define t1 (hash-ref srs-norm-shapes/color+rot '(T . 1)))
  (define t2 (hash-ref srs-norm-shapes/color+rot '(T . 2)))
  (define t3 (hash-ref srs-norm-shapes/color+rot '(T . 3)))
  (define i0 (hash-ref srs-norm-shapes/color+rot '(I . 0)))
  (define i1 (hash-ref srs-norm-shapes/color+rot '(I . 1)))
  (define l0 (hash-ref srs-norm-shapes/color+rot '(L . 0)))
  (define l1 (hash-ref srs-norm-shapes/color+rot '(L . 1)))
  (define l2 (hash-ref srs-norm-shapes/color+rot '(L . 2)))
  (define l3 (hash-ref srs-norm-shapes/color+rot '(L . 3)))
  (define j0 (hash-ref srs-norm-shapes/color+rot '(J . 0)))
  (define j1 (hash-ref srs-norm-shapes/color+rot '(J . 1)))
  (define j2 (hash-ref srs-norm-shapes/color+rot '(J . 2)))
  (define j3 (hash-ref srs-norm-shapes/color+rot '(J . 3)))

  (check-equal? (norm-shape-num-rows s0) 2)
  (check-equal? (norm-shape-num-cols s0) 3)
  (check-equal? (for*/list ([i (in-range 2)] [j (in-range 3)])
                  (norm-shape-hit-ref s0 i j))
                '(#t #t #f
                  #f #t #t))

  (check-equal? (norm-shape-num-rows t3) 3)
  (check-equal? (norm-shape-num-cols t3) 2)
  (check-equal? (for*/list ([i (in-range 3)] [j (in-range 2)])
                  (norm-shape-hit-ref t3 i j))
                '(#f #t
                  #t #t
                  #f #t))

  (define (ns-floors ns)
    (build-vector (norm-shape-num-cols ns) (curry norm-shape-floor ns)))
  (check-equal? (ns-floors s0) #(0 0 1))
  (check-equal? (ns-floors z0) #(1 0 0))
  (check-equal? (ns-floors t0) #(0 0 0))
  (check-equal? (ns-floors t2) #(1 0 1))
  (check-equal? (ns-floors l2) #(0 1 1))
  (check-equal? (ns-floors t1) #(0 1))
  (check-equal? (ns-floors i0) #(0 0 0 0))
  (check-equal? (ns-floors i1) #(0))

  (define (ns-coords ns)
    (for/list ([ij (in-norm-shape-coords ns 100 200)])
      (cons (- (car ij) 100)
            (- (cdr ij) 200))))
  (check-equal? (ns-coords s0) '((0 . 0) (0 . 1) (1 . 1) (1 . 2)))
  (check-equal? (ns-coords z0) '((0 . 1) (0 . 2) (1 . 0) (1 . 1)))
  (check-equal? (ns-coords t2) '((0 . 1) (1 . 0) (1 . 1) (1 . 2)))
  (check-equal? (ns-coords i0) '((0 . 0) (0 . 1) (0 . 2) (0 . 3)))
  (check-equal? (ns-coords i1) '((0 . 0) (1 . 0) (2 . 0) (3 . 0)))

  (check-eq? s0 (hash-ref srs-norm-shapes/color+rot '(S . 2)))
  (check-eq? z0 (hash-ref srs-norm-shapes/color+rot '(Z . 2)))
  (check-equal? (length (hash-ref srs-norm-shapes/color 'O)) 1)
  (check-equal? (length (hash-ref srs-norm-shapes/color 'S)) 2)
  (check-equal? (length (hash-ref srs-norm-shapes/color 'Z)) 2)
  (check-equal? (length (hash-ref srs-norm-shapes/color 'T)) 4)
  (check-equal? (length (hash-ref srs-norm-shapes/color 'I)) 2)
  (check-equal? (length (hash-ref srs-norm-shapes/color 'L)) 4)
  (check-equal? (length (hash-ref srs-norm-shapes/color 'J)) 4)
  (check-equal? (hash-count srs-norm-shapes/color+rot) (* 7 4)))

;; ---------------------------------------------------------------------------------------
;; finesse

;; norm-shape nat -> [listof input]
(define (inputs/column ns j)
  (argmin length
          (for/list ([ri (in-list (norm-shape-rot-infos ns))])
            (define rot (car ri))
            (define j0 (cdr ri))
            (append (rotation->inputs rot)
                    (h-offset->inputs (- j j0))))))

;; (between 0 3) -> [listof input]
(define (rotation->inputs r)
  (case r
    [(0) '()]
    [(1) '(cw)]
    [(2) '(cw cw)] ; or ccw...?
    [(3) '(ccw)]))

;; int -> [listof input]
(define (h-offset->inputs dx)
  (if (negative? dx)
      (make-list (- dx) 'left)
      (make-list dx 'right)))

;; ==========================================

(module+ test
  (define (ns->str ns)
    (format "~a~a"
            (norm-shape-color ns)
            (map car (norm-shape-rot-infos ns))))

  (for ([ns (list s0 z0 t0 j0 l0)])
    (check-equal? (for/list ([j (in-range 8)]) (inputs/column ns j))
                  '{(left left left)
                    (left left)
                    (left)
                    ()
                    (right)
                    (right right)
                    (right right right)
                    (right right right right)}
                  (ns->str ns)))

  ; 3-wide at 180 rot
  (for ([ns (list t2 j2 l2)])
    (check-equal? (for/list ([j (in-range 8)]) (inputs/column ns j))
                  '{(cw cw left left left)
                    (cw cw left left)
                    (cw cw left)
                    (cw cw)
                    (cw cw right)
                    (cw cw right right)
                    (cw cw right right right)
                    (cw cw right right right right)}
                  (ns->str ns)))

  ; 2-wide at cw rot
  (for ([ns (list t1 j1 l1)])
    (check-equal? (for/list ([j (in-range 9)]) (inputs/column ns j))
                  `{(cw left left left left)
                    (cw left left left)
                    (cw left left)
                    (cw left)
                    (cw)
                    (cw right)
                    (cw right right)
                    (cw right right right)
                    (cw right right right right)}
                  (ns->str ns)))

  ; 2-wide at ccw rot
  (for ([ns (list t3 j3 l3)])
    (check-equal? (for/list ([j (in-range 9)]) (inputs/column ns j))
                  `{(ccw left left left)
                    (ccw left left)
                    (ccw left)
                    (ccw)
                    (ccw right)
                    (ccw right right)
                    (ccw right right right)
                    (ccw right right right right)
                    (ccw right right right right right)}
                  (ns->str ns)))

  ; s/z finesse
  (for ([ns (list s1 z1)])
    (check-equal? (for/list ([j (in-range 9)]) (inputs/column ns j))
                  '{(ccw left left left)
                    (ccw left left)
                    (ccw left)
                    (ccw)
                    (cw)
                    (cw right)
                    (cw right right)
                    (cw right right right)
                    (cw right right right right)}
                  (ns->str ns)))

  ; i finesse
  (check-equal? (inputs/column i0 3) '())
  (check-equal? (inputs/column i0 0) '(left left left))
  (check-equal? (inputs/column i1 4) '(ccw))
  (check-equal? (inputs/column i1 5) '(cw)))

;; ---------------------------------------------------------------------------------------
;; operations w/ mat's

;; norm-shape mat col-nat -> row-nat
(define (sonic-drop ns mat j0)
  (apply max
         (for/list ([j (in-range (norm-shape-num-cols ns))])
           (- (mat-peak mat (+ j0 j))
              (norm-shape-floor ns j)))))

;; mat norm-shape row-nat col-nat -> mat
(define (blit-shape mat ns i0 j0)
  (mat-blit mat (in-norm-shape-coords ns i0 j0)))

(module+ test
  ;; . . . . .
  ;; . G G . .
  ;; . G G G G
  (define mat
    (make-mat '(#(#f G  G  G  G)
                #(#f G  G  #f #f))))

  (check-equal? (sonic-drop i0 mat 0) 2)
  (check-equal? (sonic-drop i1 mat 0) 0)
  (check-equal? (sonic-drop i1 mat 1) 2)

  (check-equal? (sonic-drop z0 mat 2) 1)
  (check-equal? (sonic-drop z0 mat 1) 2)
  (check-equal? (sonic-drop z1 mat 0) 1)
  (check-equal? (sonic-drop z1 mat 1) 2)
  (check-equal? (sonic-drop z1 mat 2) 2)
  (check-equal? (sonic-drop z1 mat 3) 1)

  (check-equal? (sonic-drop l2 mat 0) 1)
  (check-equal? (sonic-drop l2 mat 2) 2)
  (check-equal? (sonic-drop l1 mat 1) 2)

  ;; . . . . .
  ;; . . S . .
  ;; . . S S .
  ;; . G G S .
  ;; . G G G G
  (define mat+S
    (make-mat '(#(#f G  G  G  G)
                #(#f G  G  S  #f)
                #(#f #f S  S  #f)
                #(#f #f S  #f #f))))
  (define mat+S*
    (blit-shape mat s1 1 2))

  (check-equal? (mat->list mat+S*)
                (mat->list mat+S))

  ;; . . . . . . .
  ;; G G G . G G G
  (set! mat (make-mat '(#(G G G #f G G G))))
  (let* ([ns j1]
         [col 3]
         [row (sonic-drop ns mat col)]
         [mat (blit-shape mat ns row col)])
    (check-equal? row 0)
    (check-equal? (mat->list mat)
                  ;; . . . J J . .
                  ;; . . . J . . .
                  ;; G G G J G G G
                  '(#t #t #t #t #t #t #t
                    #f #f #f #t #f #f #f
                    #f #f #f #t #t #f #f))
    (check-equal? (mat->list (mat-clear-lines mat))
                  ;; . . . J J . .
                  ;; . . . J . . .
                  '(#f #f #f #t #f #f #f
                    #f #f #f #t #t #f #f))))

;; ---------------------------------------------------------------------------------------
;; srs

(define srs-norm-shapes
  (vector (make-norm-shape 'S 3 #(#t #t #f #f #t #t) '([0 . 3] [2 . 3]))
          (make-norm-shape 'S 2 #(#f #t #t #t #t #f) '([1 . 4] [3 . 3]))
          (make-norm-shape 'Z 3 #(#f #t #t #t #t #f) '([0 . 3] [2 . 3]))
          (make-norm-shape 'Z 2 #(#t #f #t #t #f #t) '([1 . 4] [3 . 3]))
          (make-norm-shape 'T 3 #(#t #t #t #f #t #f) '([0 . 3]))
          (make-norm-shape 'T 2 #(#t #f #t #t #t #f) '([1 . 4]))
          (make-norm-shape 'T 3 #(#f #t #f #t #t #t) '([2 . 3]))
          (make-norm-shape 'T 2 #(#f #t #t #t #f #t) '([3 . 3]))
          (make-norm-shape 'O 2 #(#t #t #t #t)       '([0 . 4] [1 . 4] [2 . 4] [3 . 4]))
          (make-norm-shape 'I 4 #(#t #t #t #t)       '([0 . 3] [2 . 3]))
          (make-norm-shape 'I 1 #(#t #t #t #t)       '([1 . 5] [3 . 4]))
          (make-norm-shape 'L 3 #(#t #t #t #f #f #t) '([0 . 3]))
          (make-norm-shape 'L 2 #(#t #t #t #f #t #f) '([1 . 4]))
          (make-norm-shape 'L 3 #(#t #f #f #t #t #t) '([2 . 3]))
          (make-norm-shape 'L 2 #(#f #t #f #t #t #t) '([3 . 3]))
          (make-norm-shape 'J 3 #(#t #t #t #t #f #f) '([0 . 3]))
          (make-norm-shape 'J 2 #(#t #f #t #f #t #t) '([1 . 4]))
          (make-norm-shape 'J 3 #(#f #f #t #t #t #t) '([2 . 3]))
          (make-norm-shape 'J 2 #(#t #t #t #f #t #f) '([3 . 3]))))

;; [hash mino-color => [listof norm-shape]]
(define srs-norm-shapes/color
  (let ([hsh (make-hasheq '())])
    (for ([ns (in-vector srs-norm-shapes)])
      (hash-update! hsh
                    (norm-shape-color ns)
                    (λ (nss) (cons ns nss))
                    (λ () '())))
    hsh))

;; [hash (cons mino-color (between 0 3)) => norm-shape]
(define srs-norm-shapes/color+rot
  (for*/hash ([ns (in-vector srs-norm-shapes)]
              [ri (in-list (norm-shape-rot-infos ns))])
    (define color (norm-shape-color ns))
    (define rot (car ri))
    (values (cons color rot) ns)))
