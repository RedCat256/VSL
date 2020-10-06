#lang racket

(require "types.rkt")
(require "util.rkt")

(provide parser%)

(define parser%
  (class object%
    (init-field tokens)
    (super-new)

    (field [pos 0]
           [cur (peek)]
           [prev cur]
           [depth 0]      ; 0 represents top level
           [inClass #f]   ; in/out class
           [loop-depth 0] ; outside loop
           )
    
    (define/private (peek-next)
      (if (>= (add1 pos) (length tokens))
          nil
          (list-ref tokens (add1 pos))))

    (define/private (peek)
      (if (>= pos (length tokens))
          nil
          (list-ref tokens pos)))

    (define/private (next)
      (set! prev cur)
      (incf pos)
      (set! cur (peek)))

    (define/private (consume type msg)
      (if (check type)
          (next)
          (parse-error msg)))
    
    (define/private (_check tok type)
      (and (empty-token? tok) (eq? (empty-token-type tok) type)))

    (define/private (check type)
      (_check cur type))

    (define/private (~check type)
      (not (check type)))

    (define/private (_match . types)
      (call/cc
       (λ (return)
         (for ([type types])
           (when (eq? type (empty-token-type cur))
             (next)
             (return #t)))
         #f)))

    (define/private (not-at-end?)
      (~check 'eof))

    (define/private (synchronize)
      (next)
      (call/cc
       (λ (return)
         (while (not-at-end?)
           (when (eq? (empty-token-type prev) '|;|)
             (return nil))
           (case (empty-token-type cur)
             [(class fun var for if while return) (return nil)])
           (next)))))

    (define/public (stmts)
      (let ([sts '()]
            [line (empty-token-line prev)])
        (while (not-at-end?)
          (set! sts (cons (declaration) sts)))
        (stmt:stmts (empty-token 'sts line) (reverse sts))))

    (define/private (declaration)
      (cond [(_match 'var)   (var)]
            [(and (check 'fun) (_check (peek-next) 'id)) (next) (fun "function")]
            [(_match 'class) (_class)]
            [else (stmt)]))

    (define/private (var)
      (let ([id cur]
            [initializer nil])
        (consume 'id "Expect variable name.")
        (when (_match '=)
          (set! initializer (expr)))
        (consume '|;| "Expect ';' after variable declaration.")
        (stmt:var id initializer)))

    (define/private (_fun kind)
      (let-values ([(tok name plist body) (values prev "anonymous" '() #f)])
      
        (incf depth) ; enter function/method

        (unless (check '|(|)
          (consume 'id (format "Expect ~a name." kind))
          (set! name (token-value prev)))
    
        (consume '|(| (format "Expect '(' after ~a name." kind))
        (while (and (~check '|)|) (not-at-end?))
          (consume 'id "Expect variable for arguments.")
          (set! plist (cons (token-value prev) plist))
          (unless (check '|)|)
            (consume '|,| "Expect ',' or ')' after parameter.")))
        (when (eq? (empty-token-type prev) '|,|)
          (parse-error "Expect parameter name after ','."))
        (set! plist (reverse plist))
        (consume '|)| "Expect ')' after parameters.")
        (consume '|{| (format "Expect '{' before ~a body." kind))
        (set! body (block-stmt))
      
        (incf depth -1) ; exit function/method

        (list tok name plist body)))

    (define/private (fun kind)
      (apply stmt:fun (_fun kind)))

    (define/private (anonymous-fun)
      (apply expr:anonymous-fun (_fun "anonymous")))

    (define/private (_class)
      (let ([name cur] [methods '()] [superClass nil])

        (set! inClass #t) ; enter class
      
        (consume 'id "Expect class name.")

        (when (_match '<)
          (consume 'id "Expect superclass name.")
          (set! superClass prev))
        
        (consume '|{| "Expect '{' after class name.")
        (while (and (~check '|}|) (not-at-end?))
          (set! methods (cons (fun "method") methods)))
        (consume '|}| "Expect '}' after class declaration.")

        (set! inClass #f) ; exit class
      
        (stmt:class name superClass (reverse methods))))

    (define/private (stmt)
      (cond [(_match '|{|)    (block-stmt)]
            [(_match 'if)     (if-stmt)]
            [(_match 'while)  (while-stmt)]
            [(_match 'for)    (for-stmt)]
            [(_match 'return) (return-stmt)]
            [(_match 'break)  (break-stmt)]
            [else             (expr-stmt)]))

    (define/private (block-stmt)
      (let ([sts '()]
            [tok prev])
        (while (and (~check '|}|) (not-at-end?))
          (set! sts (cons (declaration) sts)))
        (consume '|}| "Expect '}' after block.")
        (stmt:block tok (reverse sts))))

    (define/private (if-stmt)
      (let-values ([(tok condition if-arm then-arm)
                    (values prev #f #f #f)])
        (consume '|(| "Expect '(' after if.")
        (set! condition (expr))
        (consume '|)| "Expect ')' after if condition.")
        (set! if-arm (stmt))
        (when (_match 'else)
          (set! then-arm (stmt)))
        (stmt:if tok condition if-arm then-arm)))

    (define/private (while-stmt)
      (let-values ([(tok condition body) (values prev #f #f)])
        (incf loop-depth)
        (consume '|(| "Expect '(' after while.")
        (set! condition (expr))
        (consume '|)| "Expect ')' after while condition.")
        (set! body (stmt))
        (incf loop-depth -1)
        (stmt:while tok condition body)))

    (define/private (for-stmt)
      (let-values ([(tok init condition increment body)
                    (values prev #f #f #f #f)])
        (incf loop-depth)
        (consume '|(| "Expect '(' after for.")
        (cond [(_match 'var) (set! init (var))]
              [(_match '|;|) (set! init #f)]
              [else (set! init (expr-stmt))])
        (unless (check '|;|)
          (set! condition (expr)))
        (consume '|;| "Expect ';' after loop condition.")
        (unless (check '|)|)
          (set! increment (expr)))
        (consume '|)| "Expect ')' after for clauses.")
        (set! body (stmt))
        (incf loop-depth -1)
        (stmt:for tok init condition increment body)))

    (define/private (return-stmt)
      (when (zero? depth)
        (parse-error "Cannot return from top level code."))
      (let ([tok prev] [val nil])
        (unless (check '|;|)
          (set! val (expr)))
        (consume '|;| "Expect ';' after return value.")
        (stmt:return tok val)))

    (define/private (break-stmt)
      (when (<= loop-depth 0)
        (parse-error "'break' statement outside of loop."))
      (let ([tok prev])
        (consume '|;| "Expect ';' after break.")
        (stmt:break tok)))
      
    (define/private (expr-stmt)
      (let ([tok cur]
            [e (expr)])
        (consume '|;| "Expect ';' after expression.")
        (stmt:expr tok e)))

    (define/private (parse-prec prec)
      (let ([left (unary)])
        (while (< prec (get-prec cur))
          (next)
          (set! left (binary left)))
        left))

    (define/private (unary)
      (let ([type (empty-token-type cur)])
        (case type
          [(- !) (next) (expr:unary prev (parse-prec 120))]
          [(number id string true false nil) (next) prev]
          [(|(|) (next) (begin0 (expr:unary prev (expr)) (consume '|)| "Expect ')' for grouping"))]
          [(|[|) (next) (parse-list)]
          [(this)  (cond [inClass (next) (expr:this prev)]
                         [else (parse-error "Cannot use 'this' outside of a class.")])]
          [(super) (cond [inClass (next) (consume '|.| "Expect '.' after super.")
                                  (consume 'id "Expect identifier after '.'")
                                  (expr:super prev)]
                         [else (parse-error "Cannot use 'super' outside of a class.")])]
          [(fun) (next) (anonymous-fun)]
          [else (parse-error "Expect expression.")])))

    (define/private (parse-list)
      (let [(tok prev) (lst '())]
        (while (and (~check '|]|) (not-at-end?))
          (set! lst (cons (expr) lst))
          (unless (check '|]|)
            (consume '|,| "Expect ',' or ')' after list element.")))
        (when (eq? (empty-token-type prev) '|,|)
          (parse-error "Expect expression after ','."))
        (consume '|]| "Expect ']' after list.")
        (expr:list tok (reverse lst))))

    (define/private (arglist)
      (let ([alist '()])
        (while (and (~check '|)|) (not-at-end?))
          (set! alist (cons (expr) alist))
          (unless (check '|)|)
            (consume '|,| "Expect ',' or ')' after argument.")))
        (when (eq? (empty-token-type prev) '|,|)
          (parse-error "Expect expression after ','."))
        (reverse alist)))

    (define/private (assign target)
      (let ([val (parse-prec (sub1 (get-prec prev)))])
        (cond [(expr:get? target) (expr:set (node-token target) (expr:get-receiver target) val)]
              [(expr:subscript? target) (expr:sub-set nil (expr:subscript-target target) (expr:subscript-index target) val)]
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
          [(|[|) (let ([tok prev] [index (expr)])
                   (consume '|]| "Expect ']' for subscript expression.")
                   (expr:subscript tok left index))]
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
        [(|.| |(| |[|)     130] ; skip 120 for unary
        [else          0]))

    (define/private (expr)
      (parse-prec 0))))
