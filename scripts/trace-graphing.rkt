#!/usr/bin/env racket
#lang racket
(require plot json)

(define (graph input plot-fn)
  (plot-fn
   (lines
     (for/list ([pc (in-naturals 1)]
                [ds (in-list (hash-ref input 'ds))])
       (vector pc ds)))))

(module+ main
  (for ([trace-file (in-vector (current-command-line-arguments))])
    ; (define graph-file (path-replace-extension trace-file #".png"))
    (define input (with-input-from-file trace-file read-json))
    (define frame (graph input plot-frame))
    (send frame show #t)))
