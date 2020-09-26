#lang racket
(require "types.rkt")

(provide scanner%)

(define (char-to-symbol c)
  (string->symbol (string c)))

(define scanner%
  (class object%
    (init chars)
    (super-new)
    
    (define pos 0)
    (define str chars)
    (define start 0)
    (define line 1)
    (define c (peek))
    (define prev #f)
    (define keywords (make-hash))

    (let ([keys '(and class else false fun for if nil or
                      print return super this true var while)])
      (for ([k keys])
        (hash-set! keywords k #t)))
    
    (define/public (peek)
      (if (>= pos (string-length str))
          eof
          (string-ref str pos)))

    (define/public (next)
      (when (eqv? #\newline c)
        (set! line (add1 line)))
      (set! pos (add1 pos))
      (set! prev c)
      (if (>= pos (string-length str))
          (set! c eof)
          (set! c (string-ref str pos))))

    (define/public (_match ch)
      (and (eqv? ch c) (next)))

    (define/public (_text)
      (substring str start pos))

    (define/public (numeric?)
      (and (char? c) (char-numeric? c)))

    (define/public (name-start?)
      (and (char? c) (or (eqv? c #\_) (char-alphabetic? c))))

    (define/public (name-char?)
      (or (name-start?) (numeric?)))

    (define/public (whitespace?)
      (and (char? c) (char-whitespace? c)))

    (define/public (make-string-token)
      (while (not (or (eqv? c eof) (eqv? c #\")))
        (next))
      (next) ; skip right "
      (token 'string line (_text)))

    (define/public (make-number-token)
      (while (numeric?)
        (next))
      (token 'number line (string->number (_text))))

    (define/public (make-id-token)
      (while (name-char?)
        (next))
      (let ([sym (string->symbol (_text))])
        (if (hash-ref keywords sym #f)
            (empty-token sym line)
            (token 'id line (string->symbol (_text))))))

    (define/public (skip-white)
      (while (whitespace?)
        (next)))

    (define/public (skip-comment)
      (while (not (or (eqv? c eof) (eqv? c #\newline)))
        (next)))

    (define/public (get-tokens)
      (let ([tokens '()]
            [tok (tokenize)])
        (while (not (eq? (empty-token-type tok) 'eof))
          (set! tokens (cons tok tokens))
          (set! tok (tokenize)))
        (set! tokens (cons tok tokens))
        (reverse tokens)))

    (define/public (tokenize)
      (skip-white)
      (set! start pos)
      (cond [(eof-object? c) (empty-token 'eof line)]
            [(numeric?) (make-number-token)]
            [(name-start?) (make-id-token)]
            [(eqv? c #\") (next) (make-string-token)]
            [else
             (case (char-to-symbol c)
               [(+ - * |.| |,| |;| |(| |)| |{| |}|) (next) (empty-token (char-to-symbol prev) line)]
               [(= < > !) (next)
                          (if (_match #\=)
                              (empty-token (string->symbol (_text)) line)
                              (empty-token (char-to-symbol prev) line))]
               [(/) (next)
                    (cond [(_match #\/) (skip-comment) (tokenize)]
                          [else (empty-token '/ line)])]
               [else (raise (lex-exn (format "Invalid character '~a'" c) (current-continuation-marks)))])]))))
