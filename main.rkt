#lang racket

(require "types.rkt")
(require "scanner.rkt")
(require "parser.rkt")
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
    (lambda ()
      (let ([line (read-line)])
        (while (not (eq? line eof))
          (set! str (string-append str line "\n"))
          (set! line (read-line)))
        str))))

(define (repl-loop)
  (let ((line (readline "user> ")))
    (with-handlers
        ([lex-exn? (lambda (exn) (eprintf "LexError: ~a~n" (exn-message exn)) (repl-loop))]
         [parse-exn? (lambda (exn) (eprintf "ParseError: ~a~n" (exn-message exn)) (repl-loop))])
      (cond [(eq? eof line) (newline)]
            [line (println (send (make-parser line) expr))
                  (repl-loop)]
            [else (newline)]))))

(define (banner)
  (printf "[lox]~n"))

(define (make-parser str)
  (let ([sc (new scanner% [chars str])])
    (new parser% [tokens (send sc get-tokens)])))

(define (main)
  (if file
      (send (make-parser (read-file file)) expr)
      (begin (banner)
             (repl-loop))))

(main)
