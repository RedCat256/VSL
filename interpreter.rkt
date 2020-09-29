#lang racket
(require "types.rkt")
(require "env.rkt")

(provide interpreter%)

(define interpreter%
  (class object%
    (super-new)

    (define constructor? #f)
    (define env (new env% [outer nil]))
    (define binary-ops `((- ,-) (* ,*) (/ ,divide) (< ,<) (<= ,<=) (> ,>) (>= ,>=)))

    (define/private (symbol-function sym)
      (let ([p (assoc sym binary-ops)])
        (if p (second p) #f)))

    (define/private (visit-unary a)
      (case (empty-token-type (node-token a))
        [(-) (let ([v (_eval (expr:unary-expr a))])
               (if (number? v)
                   (- v)
                   (runtime-error "Operand of '-' must be number.")))]
        [(!) (falsy? (_eval (expr:unary-expr a)))]
        [(|(|) (_eval (expr:unary-expr a))]))

    (define/private (_and left right)
      (let ([val (_eval left)])
        (if (truthy? val)
            (_eval right)
            val)))

    (define/private (_or left right)
      (let ([val (_eval left)])
        (if (falsy? val)
            (_eval right)
            val)))
          
    (define/private (visit-binary a)
      (let ([type  (empty-token-type (node-token a))]
            [left  (expr:binary-left a)]
            [right (expr:binary-right a)])
        (cond [(eq? type 'and) (_and left right)]
              [(eq? type 'or)  (_or  left right)]
              [else
               (let ([lval  (_eval left)] [rval  (_eval right)])
                 (case type
                   [(- * / < <= > >=)
                    (unless (and (number? lval) (number? rval))
                      (runtime-error "Operands of '~a' must be two numbers." type))
                    ((symbol-function type) lval rval)]
                   [(==) (equal? lval rval)]
                   [(!=) (not (equal? lval rval))]
                   [(+)  (plus lval rval)]))])))

    (define/private (call/fn fn args)
      (let ([_env  (new env% [outer (loxFunction-env fn)])]
            [last  env]
            [arity (length (loxFunction-parameters fn))]
            [return-val nil])
        (when (not (= arity (length args)))
          (runtime-error "Expected ~a arguments but got ~a." arity (length args)))
        (for ([i (loxFunction-parameters fn)]
              [j args])
          (send _env defvar i j))
        (set! env _env)
        (with-handlers
            ([return-exn? (λ (e) (set! return-val (cadr e)))])
          (visit-block (loxFunction-body fn)))
        (set! env last)
        return-val))

    (define/private (call/new klass args)
      (let* ([init #f]
             [name (format "~a instance" (loxClass-name klass))]
             [ins (loxInstance name  klass (make-hash))])
        (cond [(hash-has-key? (loxClass-methods klass) 'init) 
               (set! init (bind/this ins (hash-ref (loxClass-methods klass) 'init)))
               (set! constructor? #t)
               (call/fn init args)
               (set! constructor? #f)]
              [(not (zero? (length args))) (runtime-error "Expected ~a arguments but got ~a." 0 (length args))])
        ins))
      
    (define/private (visit-call a)
      (let ([callee (_eval (expr:call-callee a))]
            [args   (for/list ([_ (expr:call-args a)]) (_eval _))])
        (cond [(loxFunction? callee) (call/fn callee args)]
              [(loxClass? callee) (call/new callee args)]
              [else (runtime-error "Can only call functions and classes.")])))

    (define/private (bind/this ins fn)
      (let* ([env (loxFunction-env fn)]
             [new_env (new env% [outer env])]
             [name (loxFunction-name fn)]
             [parameters (loxFunction-parameters fn)]
             [body (loxFunction-body fn)])
        (send new_env defvar "this" ins)
        (loxFunction name parameters body new_env)))
        
    (define/private (visit-get a)
      (let ([receiver (_eval (expr:get-receiver a))]
            [field    (token-value (node-token a))]
            [fn #f])
        (unless (loxInstance? receiver)
          (runtime-error "Only instances have properties."))
        (cond [(instance-has? receiver field) (instance-get receiver field)]
              [(begin (set! fn (class-get receiver field)) fn) (bind/this receiver fn)]
              [else (runtime-error (format "Undefined Property '~a'." field))])))

    (define/private (visit-set a)
      (let ([receiver (_eval (expr:set-receiver a))]
            [field    (token-value (node-token a))]
            [value    (_eval (expr:set-expr a))])
        (unless (loxInstance? receiver)
          (runtime-error "Only instances have properties."))
        (instance-set receiver field value)))

    (define/private (visit-this a)
      (send env get "this"))

    (define/private (visit-super a)
      (let* ([m_name (token-value (node-token a))]
             [_this  (send env get "this")]
             [super-class (loxClass-super-class (loxInstance-klass _this))])
        (when (nil? super-class)
          (runtime-error "Cannot use 'super' in a class without superclass."))
        (let ([method (class-get super-class m_name)])
          (unless method
            (runtime-error "Undefined property '~a'." m_name))
          (set! method (bind/this _this method))
          method)))
    
    (define/private (visit-assign a)
      (let ([name  (token-value (node-token a))]
            [value (_eval (expr:assign-expr a))])
        (send env assign name value)
        value))

    (define/private (visit-stmts a)
      (for ([stmt (stmt:stmts-slist a)])
        (_eval stmt)))

    (define/private (tostr val)
      (cond [(string? val) val]
            [(number? val) (stringify (exact->inexact val))]
            [(eq? #t val)  "true"]
            [(eq? #f val)  "false"]
            [(nil? val)    "nil"]
            [(loxFunction? val) (format "<fn ~a>" (loxFunction-name val))]
            [(loxInstance? val) (loxInstance-name val)]
            [(loxClass? val)    (loxClass-name val)]
            [else "Unknown data type"]))

    (define/private (visit-print a)
      (displayln (tostr (_eval (stmt:print-expr a)))))
    
    (define/private (visit-var a)
      (let ([name (token-value (node-token a))]
            [init (stmt:var-init a)])
        (unless (nil? init)
          (set! init (_eval init)))
        (send env defvar name init)))

    (define/private (visit-id a)
      (send env get (token-value a)))

    (define/private (visit-block a)
      (let ([new_env (new env% [outer env])]
            [previous env])
        (set! env new_env)
        (with-handlers
            ([user-exn-catched? (λ (e) (print-user-error e))])
          (for ([stmt (stmt:block-slist a)])
            (_eval stmt)))
        (set! env previous)))

    (define/private (visit-if a)
      (let ([condition (stmt:if-condition a)]
            [if-arm (stmt:if-if-arm a)]
            [then-arm (stmt:if-then-arm a)])
        (if (truthy? (_eval condition))
            (_eval if-arm)
            (when then-arm
              (_eval then-arm)))))

    (define/private (visit-while a)
      (let ([condition (stmt:while-condition a)]
            [body (stmt:while-body a)])
        (with-handlers ([break-exn? (λ (e) nil)])
          (while (truthy? (_eval condition))
            (_eval body)))))

    (define/private (visit-for a)
      (let ([init      (stmt:for-init a)]
            [condition (stmt:for-condition a)]
            [increment (stmt:for-increment a)]
            [body      (stmt:for-body a)])
        (when init (_eval init))
        (with-handlers ([break-exn? (λ (e) nil)])
          (cond [(eq? condition #f)
                 (while #t
                   (_eval body)
                   (when increment (_eval increment)))]
                [else 
                 (while (truthy? (_eval condition))
                   (_eval body)
                   (when increment (_eval increment)))]))))

    (define/private (visit-fun a)
      (let ([name (stmt:fun-name a)]
            [pars (stmt:fun-parameters a)]
            [body (stmt:fun-body a)])
        (send env defvar name (loxFunction name pars body env))))

    (define/private (visit-class a)
      (let ([name (token-value (node-token a))]
            [methods (stmt:class-methods a)]
            [memory (make-hash)]
            [super-class nil]
            [super-class-name nil])
        (unless (nil? (stmt:class-super-class a))
          (set! super-class-name (token-value (stmt:class-super-class a)))
          
          (when (eq? name super-class-name)
            (runtime-error "A class cannot inherit from itself."))
          
          (set! super-class (send env get super-class-name))
          (unless (loxClass? super-class)
            (runtime-error "Superclass must be a class.")))
                      
        (for ([i methods])
          (let* ([_name (stmt:fun-name i)]
                 [pars (stmt:fun-parameters i)]
                 [body (stmt:fun-body i)]
                 [fn   (loxFunction _name pars body env)])
            (hash-set! memory _name fn)))
        (send env defvar name (loxClass name super-class memory))))

    (define/private (visit-return a)
      (when constructor?
        (runtime-error "Cannot return value from a constructor."))
      (let ([exp (stmt:return-expr a)])
        (when (not (nil? exp))
          (set! exp (_eval exp)))
        (raise `(return ,exp))))

    (define/private (visit-break a)
      (raise (break-exn)))

    (define/public (_eval a)
      (cond [(expr:unary? a)  (visit-unary a)]
            [(expr:binary? a) (visit-binary a)]
            [(expr:call? a)   (visit-call a)]
            [(expr:assign? a) (visit-assign a)]
            [(expr:get? a)    (visit-get a)]
            [(expr:set? a)    (visit-set a)]
            [(expr:this? a)   (visit-this a)]
            [(expr:super? a)  (visit-super a)]
            [(stmt:stmts? a)  (visit-stmts a)]
            [(stmt:print? a)  (visit-print a)]
            [(stmt:expr? a)   (_eval (stmt:expr-expr a))]
            [(stmt:var? a)    (visit-var a)]
            [(stmt:block? a)  (visit-block a)]
            [(stmt:if? a)     (visit-if a)]
            [(stmt:while? a)  (visit-while a)]
            [(stmt:for? a)    (visit-for a)]
            [(stmt:fun? a)    (visit-fun a)]
            [(stmt:return? a) (visit-return a)]
            [(stmt:break? a)  (visit-break a)]
            [(stmt:class? a)  (visit-class a)]
            [else (case (empty-token-type a)
                    [(number string) (token-value a)]
                    [(true)  #t]
                    [(false) #f]
                    [(nil)   nil]
                    [(id)    (visit-id a)]
                    [else    (void)])]))))
