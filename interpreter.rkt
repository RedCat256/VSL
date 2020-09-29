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

    (define/private (eval-unary a)
      (case (empty-token-type (node-token a))
        [(-) (let ([v (_eval (expr:unary-expr a))])
               (if (number? v)
                   (- v)
                   (runtime-error "Operand of '-' must be number.")))]
        [(!) (falsy? (_eval (expr:unary-expr a)))]))

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
          
    (define/private (eval-binary a)
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
          (eval-block (loxFunction-body fn)))
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
      
    (define/private (eval-call a)
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
        
    (define/private (eval-get a)
      (let ([receiver (_eval (expr:get-receiver a))]
            [field    (token-value (node-token a))]
            [fn #f])
        (unless (loxInstance? receiver)
          (runtime-error "Only instances have properties."))
        (cond [(instance-has? receiver field) (instance-get receiver field)]
              [(begin (set! fn (class-get receiver field)) fn) (bind/this receiver fn)]
              [else (runtime-error (format "Undefined Property '~a'." field))])))

    (define/private (eval-set a)
      (let ([receiver (_eval (expr:set-receiver a))]
            [field    (token-value (node-token a))]
            [value    (_eval (expr:set-expr a))])
        (unless (loxInstance? receiver)
          (runtime-error "Only instances have properties."))
        (instance-set receiver field value)))

    (define/private (eval-this a)
      (send env get "this"))

    (define/private (eval-super a)
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
    
    (define/private (eval-assign a)
      (let ([name  (token-value (node-token a))]
            [value (_eval (expr:assign-expr a))])
        (send env assign name value)
        value))

    (define/private (eval-stats a)
      (for ([stat (stat:stats-slist a)])
        (_eval stat)))

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

    (define/private (eval-print a)
      (displayln (tostr (_eval (stat:print-expr a)))))
    
    (define/private (eval-var a)
      (let ([name (token-value (node-token a))]
            [init (stat:var-init a)])
        (unless (nil? init)
          (set! init (_eval init)))
        (send env defvar name init)))

    (define/private (eval-id a)
      (send env get (token-value a)))

    (define/private (eval-block a)
      (let ([new_env (new env% [outer env])]
            [previous env])
        (set! env new_env)
        (with-handlers
            ([user-exn-catched? (λ (e) (print-user-error e))])
          (for ([stat (stat:block-slist a)])
            (_eval stat)))
        (set! env previous)))

    (define/private (eval-if a)
      (let ([condition (stat:if-condition a)]
            [if-arm (stat:if-if-arm a)]
            [then-arm (stat:if-then-arm a)])
        (if (truthy? (_eval condition))
            (_eval if-arm)
            (when then-arm
              (_eval then-arm)))))

    (define/private (eval-while a)
      (let ([condition (stat:while-condition a)]
            [body (stat:while-body a)])
        (with-handlers ([break-exn? (λ (e) nil)])
          (while (truthy? (_eval condition))
            (_eval body)))))

    (define/private (eval-for a)
      (let ([init      (stat:for-init a)]
            [condition (stat:for-condition a)]
            [increment (stat:for-increment a)]
            [body      (stat:for-body a)])
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

    (define/private (eval-fun a)
      (let ([name (stat:fun-name a)]
            [pars (stat:fun-parameters a)]
            [body (stat:fun-body a)])
        (send env defvar name (loxFunction name pars body env))))

    (define/private (eval-class a)
      (let ([name (token-value (node-token a))]
            [methods (stat:class-methods a)]
            [memory (make-hash)]
            [super-class nil]
            [super-class-name nil])
        (unless (nil? (stat:class-super-class a))
          (set! super-class-name (token-value (stat:class-super-class a)))
          
          (when (eq? name super-class-name)
            (runtime-error "A class cannot inherit from itself."))
          
          (set! super-class (send env get super-class-name))
          (unless (loxClass? super-class)
            (runtime-error "Superclass must be a class.")))
                      
        (for ([i methods])
          (let* ([_name (stat:fun-name i)]
                 [pars (stat:fun-parameters i)]
                 [body (stat:fun-body i)]
                 [fn   (loxFunction _name pars body env)])
            (hash-set! memory _name fn)))
        (send env defvar name (loxClass name super-class memory))))

    (define/private (eval-return a)
      (when constructor?
        (runtime-error "Cannot return value from a constructor."))
      (let ([exp (stat:return-expr a)])
        (when (not (nil? exp))
          (set! exp (_eval exp)))
        (raise `(return ,exp))))

    (define/private (eval-break a)
      (raise (break-exn)))

    (define/public (_eval a)
      (cond [(expr:unary? a)  (eval-unary a)]
            [(expr:binary? a) (eval-binary a)]
            [(expr:call? a)   (eval-call a)]
            [(expr:assign? a) (eval-assign a)]
            [(expr:get? a)    (eval-get a)]
            [(expr:set? a)    (eval-set a)]
            [(expr:this? a)   (eval-this a)]
            [(expr:super? a)  (eval-super a)]
            [(stat:stats? a)  (eval-stats a)]
            [(stat:print? a)  (eval-print a)]
            [(stat:expr? a)   (_eval (stat:expr-expr a))]
            [(stat:var? a)    (eval-var a)]
            [(stat:block? a)  (eval-block a)]
            [(stat:if? a)     (eval-if a)]
            [(stat:while? a)  (eval-while a)]
            [(stat:for? a)    (eval-for a)]
            [(stat:fun? a)    (eval-fun a)]
            [(stat:return? a) (eval-return a)]
            [(stat:break? a)  (eval-break a)]
            [(stat:class? a)  (eval-class a)]
            [else (case (empty-token-type a)
                    [(number string) (token-value a)]
                    [(true)  #t]
                    [(false) #f]
                    [(nil)   nil]
                    [(id)    (eval-id a)]
                    [else    (void)])]))))
