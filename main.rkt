#lang racket

(require "types.rkt")
(require "scanner.rkt")
(require "parser.rkt")
(require "interpreter.rkt")
;(require "readline.rkt") ; for unix

(define (readline prompt)
  (display prompt)
  (read-line))

(define (read-file file)
  (define str "")
  (with-input-from-file file
    (λ ()
      (let ([line (read-line)])
        (while (not (eq? line eof))
          (set! str (string-append str line "\n"))
          (set! line (read-line)))
        str))))

(define (repl-loop itr)
  (let ([line (readline "user> ")])
    (cond [(eq? eof line) (newline)]
          [line (let ([val (interpret itr line)])
                  (unless (void? val)
                    (println val)))
                (repl-loop itr)]
          [else (newline)])))

(define (banner)
  (printf "[VSL]~n"))

(define (make-parser str)
  (let ([sc (new scanner% [chars str])])
    (new parser% [tokens (send sc get-tokens)])))

(define (interpret itr str)
  (with-handlers
      ([user-exn-catched? (λ (e) (print-user-error e))])
    (send itr _eval (send (make-parser str) stmts))))

(define (main)
  (let ([itr (new interpreter%)]
        [file #f]
        [args (current-command-line-arguments)])
    (when (> (vector-length args) 0)
      (set! file (vector-ref args 0)))
    (if file
        (interpret itr (read-file file))
        (begin (banner)
               (repl-loop itr)))))

(main)
