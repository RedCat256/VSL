#lang racket
(require "types.rkt")

(provide scanner%)

(define scanner%
  (class object%
    (init-field chars)
    (super-new)
    
    (define pos 0)
    (define start 0)
    (define line 1)
    (define c (peek))
    (define prev #f)
    (define keywords (make-hash))

    (let ([keys '(and class else false fun for if nil or return super
                      this true var while break)])
      (for ([k keys])
        (hash-set! keywords k #t)))

    (define/private (lookahead n)
      (let ([_pos (+ pos n)])
        (if (>= _pos (string-length chars))
            eof
            (string-ref chars _pos))))
    
    (define/private (peek)
      (if (>= pos (string-length chars))
          eof
          (string-ref chars pos)))

    (define/private (next)
      (when (eqv? #\newline c)
        (incf line))
      (incf pos)
      (set! prev c)
      (set! c (peek)))

    (define/private (_match ch)
      (and (eqv? ch c) (next)))

    (define/private (_text)
      (substring chars start pos))

    (define/private (numeric?)
      (and (char? c) (char-numeric? c)))

    (define/private (name-start?)
      (and (char? c) (or (eqv? c #\_) (char-alphabetic? c))))

    (define/private (name-char?)
      (or (name-start?) (numeric?)))

    (define/private (whitespace?)
      (and (char? c) (char-whitespace? c)))

    (define/private (make-string-token)
      (until (set-member? (set eof #\") c)
             (next))
      (when (eqv? c eof)
        (parse-error "Unterminated string."))
      (next) ; skip right "
      (token 'string line (substring chars (add1 start) (sub1 pos))))

    (define/private (make-number-token)
      (while (numeric?)
        (next))
      (let ([nc (lookahead 1)])
        (when (and (eqv? c #\.)
                   (char? nc)
                   (char-numeric? nc))
          (next)
          (while (numeric?)
            (next))))
      (token 'number line (string->number (_text))))

    (define/private (make-id-token)
      (while (name-char?)
        (next))
      (let ([sym (string->symbol (_text))])
        (if (hash-ref keywords sym #f)
            (empty-token sym line)
            (token 'id line (string->symbol (_text))))))

    (define/private (skip-white)
      (while (whitespace?)
        (next)))

    (define/private (skip-comment)
      (until (set-member? (set eof #\newline) c)
             (next)))

    (define/public (get-tokens)
      (let ([tokens '()]
            [tok (tokenize)])
        (until (eq? (empty-token-type tok) 'eof)
               (set! tokens (cons tok tokens))
               (set! tok (tokenize)))
        (set! tokens (cons tok tokens))
        (reverse tokens)))

    (define/private (tokenize)
      (skip-white)
      (set! start pos)
      (cond [(eof-object? c) (empty-token 'eof line)]
            [(numeric?) (make-number-token)]
            [(name-start?) (make-id-token)]
            [(eqv? c #\") (next) (make-string-token)]
            [else
             (case (char-to-symbol c)
               [(+ - * |.| |,| |;| |(| |)| |[| |]| |{| |}|) (next) (empty-token (char-to-symbol prev) line)]
               [(= < > !)
                (next)
                (cond [(_match #\=) (empty-token (string->symbol (_text)) line)]
                      [(empty-token (char-to-symbol prev) line)])]
               [(/) (next)
                    (cond [(_match #\/) (skip-comment) (tokenize)]
                          [else (empty-token '/ line)])]
               [else (lex-error "Invalid character '~a'" c)])]))))
