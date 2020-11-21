#lang racket/base
(provide
 run-engine)

(require
 racket/system
 racket/string
 "./private/types.rkt")

(module+ test
  (require rackunit racket/port))

;; --------------------------------------------------------------------------------=------
;; runners

;; -> path-string
(define (get-blockfish-engine)
  (or (getenv "BLOCKFISH_ENGINE")
      (error 'get-blockfish-engine "BLOCKFISH_ENGINE not set in environment")))

;; snapshot -> [listof [listof inputs]]
(define (run-engine ss)
  (define suggestions
    (sort (run-engine/exec ss)
          suggestion<?))
  (log-suggestions suggestions)
  (map suggestion-inputs suggestions))

;; snapshot path-string -> [listof suggestion]
(define (run-engine/exec ss [program (get-blockfish-engine)])
  (define-values [sp stdout stdin _stderr]
    (subprocess #f #f (current-output-port) program))
  (begin0 (run-engine/io ss stdout stdin)
    (close-output-port stdin)
    (close-input-port stdout)
    (subprocess-wait sp)))

;; snapshot input-port output-port -> [listof suggestion]
(define (run-engine/io ss stdout stdin)
  (write-snapshot ss stdin)
  (flush-output stdin)
  (for/list ([ln (in-port read-line stdout)])
    #:break (string=? ln "")
    (string->suggestion ln)))

;; --------------------------------------------------------------------------------=------
;; serialization and deserialization

;; snapshot output-port -> void
(define (write-snapshot ss [port (current-output-port)])
  (define (cell->string c)
    (if c
        (symbol->string c)
        "."))
  (fprintf port "~a\n~a\n"
           (cell->string (snapshot-hold ss))
           (string-join (map cell->string (snapshot-queue ss)) ""))
  (for ([row (in-list (snapshot-matrix ss))])
    (for ([cell (in-vector row)])
      (write-string (cell->string cell) port))
    (newline port))
  (newline port))

;; string -> suggestion
(define (string->suggestion s)
  (define xs (string-split s))
  (case (length xs)
    [(1) (suggestion (string->inputs (car xs)) 0)]
    [(2) (suggestion (string->inputs (car xs))
                     (or (string->number (cadr xs))
                         (error 'string->suggestion
                                (format "invalid score number: ~s" (cadr xs)))))]
    [else (error 'string->suggestion
                 (format "invalid suggestion string: ~s" s))]))

;; string -> [listof input]
(define (string->inputs s)
  (for/list ([c (in-string s)] #:when (char-alphabetic? c))
    (case c
      [(#\L) 'left]
      [(#\R) 'right]
      [(#\Z) 'ccw]
      [(#\X) 'cw]
      [(#\h) 'hold]
      [else (error 'string->inputs
                   (format "invalid input sequence ~s" s))])))

;; --------------------------------------------------------------------------------=------
;; logging

(define (suggestion<? sg1 sg2)
  (< (suggestion-score sg1)
     (suggestion-score sg2)))

;; [listof suggestion] -> void
(define (log-suggestions sgs)
  (printf "-----\n")
  (for ([i (in-naturals)]
        [sg (in-list sgs)])
    (printf "[~a] ~a\n" i (suggestion->string sg))))

;; suggestion -> string
(define (suggestion->string sg)
  (define (input->char i)
    (case i
      [(left) #\L]
      [(right) #\R]
      [(ccw) #\Z]
      [(cw) #\X]
      [(hold) #\h]))
  (format "~a; score ~a"
          (apply string (map input->char (suggestion-inputs sg)))
          (suggestion-score sg)))

;; =======================================================================================

(module+ test
  (check-equal? (string->inputs "XXLLRZh.")
                '(cw cw left left right ccw hold))
  (check-equal? (string->suggestion "XXLLRZh.")
                (suggestion '(cw cw left left right ccw hold) 0))
  (check-equal? (string->suggestion "XXLLRZh. 4")
                (suggestion '(cw cw left left right ccw hold) 4))

  (check-equal? (with-output-to-string
                  (λ () (write-snapshot
                         (snapshot '(#(#f L L L #f #f)
                                     #(#f L S S #f #f)
                                     #(#f S S T T T)
                                     #(#f #f #f #f T #f))
                                   '(ZOIJST)
                                   #f))))
                (string-append ".\n"
                               "ZOIJST\n"
                               ".LLL..\n"
                               ".LSS..\n"
                               ".SSTTT\n"
                               "....T.\n"
                               "\n"))

  (let ([stdout (open-input-string "LLL. 2\nhRRR. 1\n\n")]
        [stdin (open-output-string)])
    (define sgs
      (run-engine/io (snapshot '(#(#f #f #f #f #f)) '(ZOIJST) 'L)
                     stdout
                     stdin))
    (check-equal? (get-output-string stdin)
                  (string-append "L\n"
                                 "ZOIJST\n"
                                 ".....\n"
                                 "\n"))
    (check-equal? sgs
                  (list (suggestion '(left left left) 2)
                        (suggestion '(hold right right right) 1)))))
