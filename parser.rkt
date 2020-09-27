#lang racket

(require "types.rkt")

(provide parser%)

(define parser%
  (class object%
    (init tokens)
    (super-new)

    (define toks tokens)
    (define pos 0)
    (define cur (peek))
    (define prev cur)
    
    (define/public (peek)
      (if (>= pos (length toks))
          nil
          (list-ref toks pos)))

    (define/public (next)
      (set! prev cur)
      (set! pos (add1 pos))
      (set! cur (peek)))

    (define/public (parse-error msg)
      (raise (parse-exn msg (current-continuation-marks))))

    (define/public (consume type msg)
      (if (check type)
          (next)
          (parse-error msg)))

    (define/public (check type)
      (eq? (empty-token-type cur) type))

    (define/public (_match . types)
      (call/cc
       (λ (return)
         (for ([type types])
           (when (eq? type (empty-token-type cur))
             (next)
             (return #t)))
         #f)))

    (define/public (not-at-end?)
      (not (check 'eof)))

    (define/public (synchronize)
      (next)
      (call/cc
       (λ (return)
         (while (not-at-end?)
           (when (eq? (empty-token-type prev) '|;|)
             (return nil))
           (case (empty-token-type cur)
             [(class fun var for if while print return) (return nil)])
           (next)))))

    (define/public (statements)
      (let ([sts '()]
            [line (empty-token-line prev)])
        (while (not-at-end?)
          (set! sts (cons (declaration) sts)))
        (stat:statements (empty-token 'sts line) (reverse sts))))

    (define/public (declaration)
      (cond [(_match 'var) (varDecl)]
            [else (statement)]))

    (define/public (varDecl)
      (let ([id cur]
            [initializer nil])
        (consume 'id "Expect variable name.")
        (when (_match '=)
          (set! initializer (expr)))
        (consume '|;| "Expect ';' after variable declaration.")
        (stat:var id initializer)))

    (define/public (statement)
      (cond [(_match 'print) (print-statement)]
            [(_match '|{|) (block-statement)]
            [(_match 'if) (if-statement)]
            [(_match 'while) (while-statement)]
            [(_match 'for) (for-statement)]
            [else (expr-statement)]))

    (define/public (print-statement)
      (let ([tok prev]
            [e (expr)])
        (consume '|;| "Expect ';' after value.")
        (stat:print tok e)))

    (define/public (block-statement)
      (let ([sts '()]
            [tok prev])
        (while (and (not (check '|}|)) (not-at-end?))
          (set! sts (cons (declaration) sts)))
        (consume '|}| "Expect '}' after block.")
        (stat:block tok (reverse sts))))

    (define/public (if-statement)
      (define tok prev)
      (define condition #f)
      (define if-arm #f)
      (define then-arm #f)
      (consume '|(| "Expect '(' after if.")
      (set! condition (expr))
      (consume '|)| "Expect ')' after if condition.")
      (set! if-arm (statement))
      (when (_match 'else)
        (set! then-arm (statement)))
      (stat:if tok condition if-arm then-arm))

    (define/public (while-statement)
      (define tok prev)
      (define condition #f)
      (define body #f)
      (consume '|(| "Expect '(' after while.")
      (set! condition (expr))
      (consume '|)| "Expect ')' after while condition.")
      (set! body (statement))
      (stat:while tok condition body))

    (define/public (for-statement)
      (define tok prev)
      (define init #f)
      (define condition #f)
      (define step #f)
      (define body #f)
      (consume '|(| "Expect '(' after for.")
      (cond [(_match 'var) (set! init (varDecl))]
            [(_match '|;|) (set! init #f)]
            [else (set! init (expr-statement))])
      (unless (check '|;|)
        (set! condition (expr)))
      (consume '|;| "Expect ';' after loop condition.")
      (unless (check '|)|)
        (set! step (expr)))
      (consume '|)| "Expect ')' after for clauses.")
      (set! body (statement))
      (stat:for tok init condition step body))
      
    (define/public (expr-statement)
      (let ([tok cur]
            [e (expr)])
        (consume '|;| "Expect ';' after expression.")
        (stat:expr tok e)))

    (define/public (parse-prec prec)
      (define left (unary))
      (while (< prec (get-prec cur))
        (next)
        (set! left (binary left)))
      left)

    (define/public (unary)
      (define type (empty-token-type cur))
      (case type
        [(+ - !) (next) (expr:unary prev (parse-prec 120))]
        [(number id string true false nil) (next) prev]
        [(|(|) (next) (begin0 (expr) (consume '|)| "Expect ')' for grouping"))]
        [else (parse-error (format "Invalid unary operator '~a'" type))]))

    (define/public (binary left)
      (define type (empty-token-type prev))
      (case type
        [(or and == != < > <= >= + - * /)
         (expr:binary prev left (parse-prec (get-prec prev)))]
        [(=) (expr:binary prev left (parse-prec (sub1 (get-prec prev))))]
        [else (parse-error (format "Invalid binary operator '~a'" type))]))
       
    (define/public (get-prec tok)
      (case (empty-token-type tok)
        [(=) 50]
        [(or) 60]
        [(and) 70]
        [(== !=) 80]
        [(< > <= >=) 90]
        [(+ -) 100]
        [(* /) 110]
        [else 0]))

    (define/public (expr)
      (parse-prec 0))))
