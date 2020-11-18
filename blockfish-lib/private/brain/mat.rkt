#lang racket/base
(provide
 mat?
 make-mat
 mat-num-rows
 mat-num-cols
 mat-num-cells
 mat-ref
 mat-peak
 mat-blit
 mat-clear-lines
 mat->list
 display-mat)

(require
 racket/vector
 racket/function)

(module+ test
  (require rackunit racket/port))

;; ---------------------------------------------------------------------------------------
;; matrix

;; num-cols, num-rows, num-cells : nat
;; bits : bits
;; peaks : (vectorof nat)
(struct mat
  [num-rows
   num-cols
   num-cells
   bits
   peaks])

;; (and [listof vector] pair) -> mat
(define (make-mat rows)
  (define num-cols (vector-length (car rows)))
  (define bits
    (for/sum ([i (in-naturals)]
              [row (in-list rows)])
      (arithmetic-shift (for/sum ([j (in-naturals)]
                                  [cell (in-vector row)])
                          (if cell
                              (arithmetic-shift 1 j)
                              0))
                        (* i num-cols))))
  (define num-rows
    (or (for/last ([i (in-naturals)])
          #:break (bits-row-empty? bits num-cols i)
          (add1 i))
        0))
  (define num-cells
    (bits-count bits))
  (define peaks
    (build-vector num-cols (curry bits-col-height bits num-rows num-cols)))
  (mat num-rows num-cols num-cells bits peaks))

;; mat nat nat -> boolean
(define (mat-ref m i j)
  (define num-cols (mat-num-cols m))
  (if (and (>= i 0)
           (>= j 0)
           (< j num-cols))
      (bits-ref (mat-bits m) num-cols i j)
      #t))

;; mat -> [listof boolean]
(define (mat->list m)
  (for*/list ([i (in-range (mat-num-rows m))]
              [j (in-range (mat-num-cols m))])
    (mat-ref m i j)))

;; mat -> void
(define (display-mat m [port (current-output-port)])
  (for ([i* (in-range (mat-num-rows m))])
    (define space "")
    (fprintf port "|")
    (define i (- (mat-num-rows m) i* 1))
    (for ([j (in-range (mat-num-cols m))])
      (fprintf port "~a~a"
               space
               (if (mat-ref m i j) "x" "."))
      (set! space " "))
    (fprintf port "|\n")))

;; mat col-nat -> nat
(define (mat-peak m j)
  (vector-ref (mat-peaks m) j))

;; mat [sequenceof (cons row-nat col-nat)] -> mat
(define (mat-blit m coords-seq)
  (define num-cols (mat-num-cols m))
  (define peaks (vector-copy (mat-peaks m)))
  (define-values [num-rows num-cells bits]
    (for*/fold ([num-rows (mat-num-rows m)]
                [num-cells (mat-num-cells m)]
                [bits (mat-bits m)])
               ([ij coords-seq]
                [i (in-value (car ij))]
                [j (in-value (cdr ij))]
                #:when (not (bits-ref bits num-cols i j)))
      (vector-set! peaks j (max (vector-ref peaks j) (add1 i)))
      (values (max num-rows (add1 i))
              (add1 num-cells)
              (bits-set bits num-cols i j))))
  (mat num-rows num-cols num-cells bits peaks))

;; mat -> mat
;; optional: #:out [boxof [listof row-nat]]
(define (mat-clear-lines m #:out [out #f])
  (define num-cols (mat-num-cols m))
  (define bits (mat-bits m))
  (define-values [num-rows num-cells cleared/desc]
    (for/fold ([num-rows (mat-num-rows m)]
               [num-cells (mat-num-cells m)]
               [cleared/desc '()])
              ([i (in-range (mat-num-rows m))])
      (if (bits-row-full? bits num-cols i)
          (values (sub1 num-rows)
                  (- num-cells num-cols)
                  (cons i cleared/desc))
          (values num-rows
                  num-cells
                  cleared/desc))))
  (define bits*
    (for/fold ([bits bits])
              ([i (in-list cleared/desc)])
      (bits-del-row bits num-cols i)))
  (define peaks*
    (build-vector num-cols (curry bits-col-height bits* num-rows num-cols)))
  (when (box? out)
    (set-box! out (reverse cleared/desc)))
  (mat num-rows num-cols num-cells bits* peaks*))

;; =======================================================================================

(module+ test
  (define (peaks m)
    (for/list ([j (in-range (mat-num-cols m))])
      (mat-peak m j)))

  (define m0 (make-mat '(#(#f #f #f #f))))
  (check-equal? (mat-num-cols m0) 4)
  (check-equal? (mat-num-rows m0) 0)
  (check-equal? (mat-num-cells m0) 0)
  (check-equal? (peaks m0) '(0 0 0 0))

  ;; . . . .
  ;; J . . .
  ;; J J J .
  (define mJ (make-mat '(#(J  J  J  #f)
                         #(J  #f #f #f)
                         #(#f #f #f #f))))
  (check-equal? (mat-num-cols mJ) 4)
  (check-equal? (mat-num-rows mJ) 2)
  (check-equal? (mat-num-cells mJ) 4)
  (check-equal? (peaks mJ) '(2 1 1 0))

  ;; . . Z .
  ;; . Z Z .
  ;; J Z . .
  ;; J J J .
  (define mJZ (make-mat '(#(J  J  J  #f)
                          #(J  Z  #f #f)
                          #(#f Z  Z  #f)
                          #(#f #f Z  #f))))
  (check-equal? (mat-num-cols mJZ) 4)
  (check-equal? (mat-num-rows mJZ) 4)
  (check-equal? (mat-num-cells mJZ) 8)
  (check-equal? (mat->list mJZ)
                '(#t #t #t #f
                  #t #t #f #f
                  #f #t #t #f
                  #f #f #t #f))
  (check-equal? (peaks mJZ) '(2 3 4 0))

  (check-equal? (with-output-to-string
                  (λ () (display-mat mJZ)))
                (string-append "|. . x .|\n"
                               "|. x x .|\n"
                               "|x x . .|\n"
                               "|x x x .|\n"))

  ;; same as above but by blitting individual coords
  (define mJZ* (mat-blit mJ (in-list '((1 . 1) (2 . 1) (2 . 2) (3 . 2)))))
  (check-equal? (mat-num-cols mJZ*) 4)
  (check-equal? (mat-num-rows mJZ*) 4)
  (check-equal? (mat-num-cells mJZ*) 8)
  (check-equal? (mat->list mJZ) (mat->list mJZ*))
  (check-equal? (peaks mJZ) (peaks mJZ*))

  (define the-lines (box '()))

  ;; . . Z .
  ;; X Z Z X <---- cleared
  ;; J Z . .
  ;; J J J .
  (define m-lc (mat-blit mJZ '((2 . 0) (2 . 3))))
  (check-equal? (mat-num-rows m-lc) 4)
  (check-equal? (mat-num-cells m-lc) 10)
  (check-equal? (peaks m-lc) '(3 3 4 3))
  (define m-lc* (mat-clear-lines m-lc #:out the-lines))
  (check-equal? (mat-num-rows m-lc*) 3)
  (check-equal? (mat-num-cells m-lc*) 6)
  (check-equal? (unbox the-lines) '(2))
  (check-equal? (peaks m-lc*) '(2 2 3 0))
  (check-equal? (mat->list m-lc*)
                '(#t #t #t #f
                  #t #t #f #f
                  #f #f #t #f))

  ;; . . Z .
  ;; X Z Z X <---- cleared
  ;; J Z . .
  ;; J J J X <---- cleared
  (define m-lc2 (mat-blit mJZ '((0 . 3) (2 . 0) (2 . 3))))
  (define m-lc2* (mat-clear-lines m-lc2 #:out the-lines))
  (check-equal? (mat-num-rows m-lc2*) 2)
  (check-equal? (mat-num-cells m-lc2*) 3)
  (check-equal? (unbox the-lines) '(0 2))
  (check-equal? (peaks m-lc2*) '(1 1 2 0))
  (check-equal? (mat->list m-lc2*)
                '(#t #t #f #f
                  #f #f #t #f)))

;; ---------------------------------------------------------------------------------------
;; bits

(define bits? exact-nonnegative-integer?)

;; bits nat row-nat col-nat -> boolean
(define (bits-ref bits num-cols i j)
  (bitwise-bit-set? bits (+ (* i num-cols) j)))

;; bits nat row-nat -> bits
(define (bits-row-bits bits num-cols i)
  (bitwise-bit-field bits
                     (* i num-cols)
                     (* (add1 i) num-cols)))

;; bits nat row-nat -> boolean
(define (bits-row-empty? bits num-cols i)
  (zero? (bits-row-bits bits num-cols i)))
(define (bits-row-full? bits num-cols i)
  (bitwise-bit-set? (add1 (bits-row-bits bits num-cols i)) ; try to overflow
                    num-cols))

;; bits nat nat col-nat -> nat
(define (bits-col-height bits num-rows num-cols j)
  (or (for/last ([i (in-range num-rows)]
                 #:when (bits-ref bits num-cols i j))
        (add1 i))
      0))

;; bits nat row-nat col-nat -> boolean
(define (bits-set bits num-cols i j)
  (bitwise-ior bits (arithmetic-shift 1 (+ (* i num-cols) j))))

;; bits nat row-nat -> bits
(define (bits-del-row bits num-cols i)
  (define lower-mask (sub1 (arithmetic-shift 1 (* i num-cols))))
  (define upper-mask (bitwise-not (sub1 (arithmetic-shift 1 (* (add1 i) num-cols)))))
  (bitwise-ior (bitwise-and bits lower-mask)
               (arithmetic-shift (bitwise-and bits upper-mask)
                                 (- num-cols))))

;; bits -> nat
(define (bits-count bits)
  (for/sum ([i (in-range (integer-length bits))])
    (if (bitwise-bit-set? bits i) 1 0)))
