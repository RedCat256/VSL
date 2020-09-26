#lang racket

(require "types.rkt")
(require "scanner.rkt")
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
    (cond [(eq? eof line) (newline)]
          [line (println (send (new scanner% [chars line]) get-tokens))
                (repl-loop)]
          [else (newline)])))

(define (banner)
  (printf "[lox]~n"))

(define (main)
  (if file
      (println (send (new scanner% [chars (read-file file)]) get-tokens))
      (begin (banner)
             (repl-loop))))

(main)
