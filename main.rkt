#lang racket

(require "types.rkt")
(require "util.rkt")
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
        (until (eq? line eof)
               (set! str (string-append str line "\n"))
               (set! line (read-line)))
        str))))

(define (repl-loop itr)
  (let ([line (readline "user> ")])
    (cond [(eq? eof line) (newline)]
          [line (let ([val (interpret itr line)])
                  (unless (void? val)
                    (displayln val)))
                (repl-loop itr)]
          [else (newline)])))

(define (banner)
  (printf "[VSL]~n"))

(define (make-parser str)
  (let* ([sc (new scanner% [chars str])]
         [toks (send sc get-tokens)]
         [parser (new parser% [tokens toks])])
        (when (get-field had-error sc)
          (set-field! had-error parser #t))
        parser))

(define (interpret itr str)
  (with-handlers
      ([runtime-exn? (λ (e) (eprintf "\x1b[1;31mRuntimeError: ~a~n\x1b[0m" (exn-message e)))])
    (let* ([parser (make-parser str)]
           [ast (send parser stmts)])
        (unless (get-field had-error parser)
          (send itr evaluate ast)))))

(define (main)
  (let ([itr nil]
        [file #f]
        [args (current-command-line-arguments)])
    (if (> (vector-length args) 0)
      (begin (set! file (vector-ref args 0))
             (set! itr (new interpreter% [in-repl #f])))
      (set! itr (new interpreter% [in-repl #t])))
    (if file
        (interpret itr (read-file file))
        (begin (banner)
               (repl-loop itr)))))

(main)
