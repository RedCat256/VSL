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
      (when (eqv? #\newline (peek))
        (set! line (add1 line)))
      (set! pos (add1 pos)))

    (define/public (_match c)
      (if (eqv? c (peek))
          (next)
          #f))

    (define/public (_text)
      (substring str start pos))

    (define/public (numeric?)
      (let ([c (peek)])
        (and (char? c) (char-numeric? c))))

    (define/public (name-start?)
      (let ([c (peek)])
        (and (char? c) (or (eqv? c #\_) (char-alphabetic? c)))))

    (define/public (name-char?)
      (or (name-start?) (numeric?)))

    (define/public (whitespace?)
      (let ([c (peek)])
        (and (char? c) (char-whitespace? c))))

    (define/public (make-string-token)
      (define c (peek))
      (while (not (or (eqv? c eof) (eqv? c #\")))
        (next)
        (set! c (peek)))
      (next) ; skip right "
      (token 'string line (_text)))

    (define/public (make-number-token)
      (define c (peek))
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

    (define/public (get-tokens)
      (let ([tokens '()]
            [tok (tokenize)])
        (while (not (eq? (empty-token-type tok) 'eof))
          (set! tokens (cons tok tokens))
          (set! tok (tokenize)))
        (set! tokens (cons tok tokens))
        (reverse tokens)))

    (define/public (tokenize)
      (define c (peek))

      (skip-white)
      (set! start pos)
      (set! c (peek))
      (cond [(eof-object? c) (empty-token 'eof line)]
            [(numeric?) (make-number-token)]
            [(name-start?) (make-id-token)]
            [(eqv? c #\") (next) (make-string-token)]
            [else
             (case (char-to-symbol c)
               [(+ - * |.| |,| |;| |(| |)| |{| |}|) (next) (empty-token (char-to-symbol c) line)]
               [(= < > !)
                (next)
                (if (_match #\=)
                    (empty-token (string->symbol (_text)) line)
                    (empty-token (char-to-symbol c) line))]
               [(/)
                (next)
                (let ([c (peek)])
                  (if (_match #\/)
                      (begin
                        (while (not (or (eqv? c eof) (eqv? c #\newline)))
                          (next)
                          (set! c (peek)))
                        (tokenize))
                      (empty-token '/ line)))]
               [else (raise (lex-exn (format "Invalid character '~a'" c) (current-continuation-marks)))])]))))
