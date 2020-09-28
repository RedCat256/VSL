#lang racket

(require "types.rkt")

(provide parser%)

(define parser%
  (class object%
    (init-field tokens)
    (super-new)

    (field [pos 0]
           [cur (peek)]
           [prev cur]
           [depth 0]      ; 0 represents top level
           [inClass #f])  ; in/out class
    
    (define/private (peek)
      (if (>= pos (length tokens))
          nil
          (list-ref tokens pos)))

    (define/private (next)
      (set! prev cur)
      (set! pos (add1 pos))
      (set! cur (peek)))

    (define/private (consume type msg)
      (if (check type)
          (next)
          (parse-error msg)))

    (define/private (check type)
      (eq? (empty-token-type cur) type))

    (define/private (_match . types)
      (call/cc
       (λ (return)
         (for ([type types])
           (when (eq? type (empty-token-type cur))
             (next)
             (return #t)))
         #f)))

    (define/private (not-at-end?)
      (not (check 'eof)))

    (define/private (synchronize)
      (next)
      (call/cc
       (λ (return)
         (while (not-at-end?)
           (when (eq? (empty-token-type prev) '|;|)
             (return nil))
           (case (empty-token-type cur)
             [(class fun var for if while print return) (return nil)])
           (next)))))

    (define/public (stats)
      (let ([sts '()]
            [line (empty-token-line prev)])
        (while (not-at-end?)
          (set! sts (cons (declaration) sts)))
        (stat:stats (empty-token 'sts line) (reverse sts))))

    (define/private (declaration)
      (cond [(_match 'var)   (var)]
            [(_match 'fun)   (fun "function")]
            [(_match 'class) (_class)]
            [else (stat)]))

    (define/private (var)
      (let ([id cur]
            [initializer nil])
        (consume 'id "Expect variable name.")
        (when (_match '=)
          (set! initializer (expr)))
        (consume '|;| "Expect ';' after variable declaration.")
        (stat:var id initializer)))

    (define/private (fun kind)
      (let-values ([(tok id plist body) (values prev cur '() #f)])
      
        (set! depth (add1 depth)) ; enter function/method
      
        (consume 'id (format "Expect ~a name." kind))
        (consume '|(| (format "Expect '(' after ~a name." kind))
        (while (and (not (check '|)|)) (not-at-end?))
          (consume 'id "Expect variable for arguments.")
          (set! plist (cons (token-value prev) plist))
          (unless (check '|)|)
            (consume '|,| "Expect ',' or ')' after parameter.")))
        (set! plist (reverse plist))
        (consume '|)| "Expect ')' after parameters.")
        (consume '|{| (format "Expect '{' before ~a body." kind))
        (set! body (block-stat))
      
        (set! depth (sub1 depth)) ; exit function/method
      
        (stat:fun tok (token-value id) plist body)))

    (define/private (_class)
      (let ([name cur] [methods '()])

        (set! inClass #t) ; enter class
      
        (consume 'id "Expect class name.")
        (consume '|{| "Expect '{' after class name.")
        (while (and (not (check '|}|)) (not-at-end?))
          (set! methods (cons (fun "method") methods)))
        (consume '|}| "Expect '}' after class declaration.")

        (set! inClass #f) ; exit class
      
        (stat:class name (reverse methods))))

    (define/private (stat)
      (cond [(_match 'print)  (print-stat)]
            [(_match '|{|)    (block-stat)]
            [(_match 'if)     (if-stat)]
            [(_match 'while)  (while-stat)]
            [(_match 'for)    (for-stat)]
            [(_match 'return) (return-stat)]
            [else             (expr-stat)]))

    (define/private (print-stat)
      (let ([tok prev]
            [e (expr)])
        (consume '|;| "Expect ';' after value.")
        (stat:print tok e)))

    (define/private (block-stat)
      (let ([sts '()]
            [tok prev])
        (while (and (not (check '|}|)) (not-at-end?))
          (set! sts (cons (declaration) sts)))
        (consume '|}| "Expect '}' after block.")
        (stat:block tok (reverse sts))))

    (define/private (if-stat)
      (let-values ([(tok condition if-arm then-arm)
                    (values prev #f #f #f)])
        (consume '|(| "Expect '(' after if.")
        (set! condition (expr))
        (consume '|)| "Expect ')' after if condition.")
        (set! if-arm (stat))
        (when (_match 'else)
          (set! then-arm (stat)))
        (stat:if tok condition if-arm then-arm)))

    (define/private (while-stat)
      (let-values ([(tok condition body) (values prev #f #f)])
        (consume '|(| "Expect '(' after while.")
        (set! condition (expr))
        (consume '|)| "Expect ')' after while condition.")
        (set! body (stat))
        (stat:while tok condition body)))

    (define/private (for-stat)
      (let-values ([(tok init condition increment body)
                    (values prev #f #f #f #f)])
        (consume '|(| "Expect '(' after for.")
        (cond [(_match 'var) (set! init (var))]
              [(_match '|;|) (set! init #f)]
              [else (set! init (expr-stat))])
        (unless (check '|;|)
          (set! condition (expr)))
        (consume '|;| "Expect ';' after loop condition.")
        (unless (check '|)|)
          (set! increment (expr)))
        (consume '|)| "Expect ')' after for clauses.")
        (set! body (stat))
        (stat:for tok init condition increment body)))

    (define/private (return-stat)
      (let ([tok prev] [val #f])
        (when (zero? depth)
          (parse-error "Cannot return from top level code."))
        (set! val (expr))
        (consume '|;| "Expect ';' after return value.")
        (stat:return tok val)))
      
    (define/private (expr-stat)
      (let ([tok cur]
            [e (expr)])
        (consume '|;| "Expect ';' after expression.")
        (stat:expr tok e)))

    (define/private (parse-prec prec)
      (let ([left (unary)])
        (while (< prec (get-prec cur))
          (next)
          (set! left (binary left)))
        left))

    (define/private (unary)
      (let ([type (empty-token-type cur)])
        (case type
          [(+ - !) (next) (expr:unary prev (parse-prec 120))]
          [(number id string true false nil) (next) prev]
          [(|(|) (next) (begin0 (expr) (consume '|)| "Expect ')' for grouping"))]
          [(this) (cond [inClass (next) (expr:this prev)]
                        [else (parse-error "Cannot use 'this' out of class.")])]
          [else (parse-error "Invalid unary operator '~a'" type)])))

    (define/private (arglist)
      (let ([alist '()])
        (while (and (not (check '|)|)) (not-at-end?))
          (set! alist (cons (expr) alist))
          (unless (check '|)|)
            (consume '|,| "Expect ',' or ')' after argument.")))
        (reverse alist)))

    (define/private (assign target)
      (let ([val (parse-prec (sub1 (get-prec prev)))])
        (cond [(expr:get? target) (expr:set (node-token target) (expr:get-receiver target) val)]
              [(and (token? target) (eq? (empty-token-type target) 'id)) (expr:assign target val)]
              [else (parse-error "Invalid assignment target.")])))

    (define/private (binary left)
      (let ([type (empty-token-type prev)])
        (case type
          [(or and == != < > <= >= + - * /)
           (expr:binary prev left (parse-prec (get-prec prev)))]
          [(=) (assign left)]
          [(|(|) (let ([alist (arglist)])
                   (consume '|)| "Expect ')' after function call.")
                   (expr:call prev left alist))]
          [(|.|) (consume 'id "Expect property name after '.'.")
                 (expr:get prev left)]
          [else (parse-error "Invalid binary operator '~a'" type)])))
       
    (define/private (get-prec tok)
      (case (empty-token-type tok)
        [(=)           50]
        [(or)          60]
        [(and)         70]
        [(== !=)       80]
        [(< > <= >=)   90]
        [(+ -)         100]
        [(* /)         110]
        [(|.| |(|)     130] ; skip 120 for unary
        [else          0]))

    (define/private (expr)
      (parse-prec 0))))
