#lang racket

(require "types.rkt")
(require "scanner.rkt")
(require "parser.rkt")
(require "interpreter.rkt")
;(require "readline.rkt") ; for unix

(define (readline prompt)
  (display prompt)
  (read-line))

(define args (current-command-line-arguments))
(define file #f)
(when (> (vector-length args) 0)
  (set! file (vector-ref args 0)))

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
    (with-handlers
        ([lex-exn? (λ (exn) (eprintf "LexError: ~a~n" (exn-message exn)) (repl-loop itr))]
         [parse-exn? (λ (exn) (eprintf "ParseError: ~a~n" (exn-message exn)) (repl-loop itr))]
         [runtime-exn? (λ (exn) (eprintf "RuntimeError: ~a~n" (exn-message exn)) (repl-loop itr))]
         [exn:fail:contract:divide-by-zero? (λ (exn) (eprintf "~a~n" (exn-message exn)) (repl-loop itr))])
      (cond [(eq? eof line) (newline)]
            [line (let ([val (interpret itr line)])
                    (unless (void? val)
                      (println val)))
                  (repl-loop itr)]
            [else (newline)]))))

(define (banner)
  (printf "[lox]~n"))

(define (make-parser str)
  (let ([sc (new scanner% [chars str])])
    (new parser% [tokens (send sc get-tokens)])))

(define (interpret itr str)
  (send itr _eval (send (make-parser str) statements)))

(define (main)
  (let ([itr (new interpreter%)])
    (if file
        (interpret itr (read-file file))
        (begin (banner)
               (repl-loop itr)))))

(main)
