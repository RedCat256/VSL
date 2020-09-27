#lang racket
(require "types.rkt")
(require "env.rkt")

(provide interpreter%)

(define interpreter%
  (class object%
    (super-new)

    (define env (new env% [outer nil]))

    (define binary-ops (list `(+ ,+) `(- ,-) `(* ,*) `(/ ,/) `(< ,<) `(<= , <=)
                             `(> ,>) `(>= ,>=) `(== ,=)))

    (define/public (symbol-function sym)
      (let ([p (assoc sym binary-ops)])
        (if p (second p) #f)))

    (define/public (eval-literal a)
      (token-value a))

    (define/public (eval-unary a)
      (define tok (node-token a))
      (case (empty-token-type tok)
        [(+) (+ (_eval (expr:unary-expr a)))]
        [(-) (- (_eval (expr:unary-expr a)))]
        [(!) (not (_eval (expr:unary-expr a)))]))

    (define/public (eval-binary a)
      (define tok (node-token a))
      (define left (expr:binary-left a))
      (define right (expr:binary-right a))
      (define type (empty-token-type tok))
      (case type
        [(+ - * / < <= > >= ==)
         ((symbol-function type) (_eval left) (_eval right))]
        [(and) (and (_eval left) (_eval right))]
        [(or) (or (_eval left) (_eval right))]
        [(=) (eval-assign a)]))

    (define/public (eval-assign a)
      (let* ([left (expr:binary-left a)]
             [name (token-value left)]
             [right (expr:binary-right a)]
             [value (_eval right)])
        (send env assign name value)
        value))

    (define/public (eval-statements a)
      (let ([r nil])
        (for ([stat (stat:statements-slist a)])
          (set! r(_eval stat)))
        r))

    (define/public (eval-print a)
      (displayln (_eval (stat:print-expr a))))

    (define/public (eval-varDecl a)
      (let ([name (token-value (node-token a))]
            [init (stat:var-init a)])
        (unless (nil? init)
          (set! init (_eval init)))
        (send env defvar name init)))

    (define/public (eval-id a)
      (send env get (token-value a)))

    (define/public (eval-block a)
      (define ne (new env% [outer env]))
      (define previous env)
      (set! env ne)
      (let ([r nil])
        (for ([stat (stat:block-slist a)])
          (set! r (_eval stat)))
        (set! env previous)
        r))

    (define/public (_eval a)
      (cond [(expr:unary? a) (eval-unary a)]
            [(expr:binary? a) (eval-binary a)]
            [(stat:statements? a) (eval-statements a)]
            [(stat:print? a) (eval-print a)]
            [(stat:expr? a) (_eval (stat:expr-expr a))]
            [(stat:var? a) (eval-varDecl a)]
            [(stat:block? a) (eval-block a)]
            [else (case (empty-token-type a)
                    [(number string) (eval-literal a)]
                    [(true) #t]
                    [(false) #f]
                    [(nil) nil]
                    [(id) (eval-id a)]
                    [else nil])]))))
    
