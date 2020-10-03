#lang racket
(require "types.rkt")
(require "env.rkt")
(require "runtime/system.rkt")
(require "runtime/ui.rkt")

(provide interpreter%)

(define runtime-natives (list ui-natives))
(define runtime-native-class (list System))

(define interpreter%
  (class object%
    (super-new)

    (define cur-fun nil) ; current activation record
    (define env (new env% [outer nil]))
    
    (define binary-ops `((- ,-) (* ,*) (/ ,divide) (< ,<) (<= ,<=) (> ,>) (>= ,>=)))

    (for ([natives runtime-natives])
      (for ([_ natives])
        (send env defvar (Native-name _) _)))
    
    (for ([_ runtime-native-class])
      (send env defvar (Class-name _) _))

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

    (define/private (visit-block a)
      (let ([prev env])
        (set! env (new env% [outer env]))
        (with-handlers
            ([user-exn-catched? (λ (e) (set! env prev) (raise e))])
          (for ([stmt (stmt:block-slist a)])
            (_eval stmt))
          (set! env prev))))

    (define/private (call/fn fn args)
      (let ([_env  (new env% [outer (Function-env fn)])]
            [last  env]
            [arity (length (Function-parameters fn))]
            [return-val nil]
            [prev-fun cur-fun])
        (when (~= arity (length args))
          (runtime-error "Expected ~a arguments but got ~a." arity (length args)))
        (for ([i (Function-parameters fn)] [j args])
          (send _env defvar i j))
        (set! env _env)
        (set! cur-fun fn)
        (with-handlers
            ([return-exn? (λ (e) (set! return-val (cadr e)))])
          (visit-block (Function-body fn)))
        (when (initializer? fn)
          (set! return-val (send env get "this")))
        (set! env last)
        (set! cur-fun prev-fun)
        return-val))

    (define/private (call/new klass args)
      (let* ([init #f]
             [name (format "~a instance" (Class-name klass))]
             [ins  (Instance name  klass (make-hash))])
        (cond [(hash-has-key? (Class-methods klass) 'init)
               (set! init (bind/this ins (hash-ref (Class-methods klass) 'init)))
               (call/fn init args)]
              [(not (zero? (length args))) (runtime-error "Expected ~a arguments but got ~a." 0 (length args))])
        ins))

    (define/private (call/native callee args)
      (if (~= (Native-arity callee) (length args))
          (runtime-error "Expected ~a arguments but got ~a." (Native-arity callee) (length args))
          (apply (Native-fn callee) args)))
      
    (define/private (visit-call a)
      (let ([callee (_eval (expr:call-callee a))]
            [args   (for/list ([_ (expr:call-args a)]) (_eval _))])
        (cond [(Function? callee) (call/fn callee args)]
              [(Class? callee)
                (if (Class-instantiable callee)
                    (call/new callee args)
                    (runtime-error "Class ~a cannot be instantiated." (Class-name callee)))]
              [(Native? callee) (call/native callee args)]
              [else (runtime-error "Can only call functions and classes.")])))

    (define/private (bind/this ins fn)
      (let ([env (new env% [outer (Function-env fn)])])
        (send env defvar "this" ins)
        (struct-copy Function fn [env env])))
        
    (define/private (visit-get a)
      (let ([receiver (_eval (expr:get-receiver a))]
            [field    (token-value (node-token a))]
            [fn #f])
        (unless (or (Instance? receiver) (Class? receiver))
          (runtime-error "Only instance or class have properties."))
        (cond [(Instance? receiver)
                (if (instance-has? receiver field)
                    (instance-get receiver field)
                    (bind/this receiver (class-get receiver field)))]
              [(Class? receiver) (class-get receiver field)]
              [else (runtime-error (format "Undefined Property '~a'." field))])))

    (define/private (visit-set a)
      (let ([receiver (_eval (expr:set-receiver a))]
            [field    (token-value (node-token a))]
            [value    (_eval (expr:set-expr a))])
        (unless (Instance? receiver)
          (runtime-error "Only instance have fields."))
        (instance-set receiver field value)
        value))

    (define/private (visit-this a)
      (send env get "this"))

    (define/private (visit-super a)
      (let* ([m_name (token-value (node-token a))]
             [_this  (send env get "this")]
             [super-class (Class-super-class (Function-klass cur-fun))])
        (when (nil? super-class)
          (runtime-error "Cannot use 'super' in a class without superclass."))
        (let ([method (class-get super-class m_name)])
          (unless method
            (runtime-error "Undefined property '~a'." m_name))
          (set! method (bind/this _this method))
          method)))

    (define/private (visit-list a)
      (let ([lst #f])
        (set! lst
              (for/list ([e (expr:list-elements a)])
                (_eval e)))
        (List lst (length lst))))

    (define/private (check-subscript target index)
      (when (not (List? target))
        (runtime-error "Can only apply '[' to a list"))
      (when (not (and (exact? index) (integer? index)))
        (runtime-error "Subscript must be an integer."))
      (when (or (>= index (List-length target)) (negative? index))
        (runtime-error "Index out of range, expect 0..~a, but got '~a'" (sub1 (List-length target)) index)))

    (define/private (visit-subscript a)
      (let ([target (_eval (expr:subscript-target a))]
            [index  (_eval (expr:subscript-index a))])
        (check-subscript target index)
        (list-ref (List-elements target) index)))

    (define/private (visit-sub-set a)
      (let ([target (_eval (expr:sub-set-target a))]
            [index  (_eval (expr:sub-set-index a))]
            [value  (_eval (expr:sub-set-expr a))])
        (check-subscript target index)
        (let ([v #f])
          (set! v (list->vector (List-elements target)))
          (vector-set! v index value)
          (set-List-elements! target (vector->list v))
          target)))

    (define/private (visit-assign a)
      (let ([name  (token-value (node-token a))]
            [value (_eval (expr:assign-expr a))])
        (send env assign name value)
        value))

    (define/private (visit-stmts a)
      (for ([stmt (stmt:stmts-slist a)])
        (_eval stmt)))
    
    (define/private (visit-var a)
      (let ([name (token-value (node-token a))]
            [init (stmt:var-init a)])
        (unless (nil? init)
          (set! init (_eval init)))
        ;persitent environments [crafting interpreters/resolving-and-binding]
        (set! env (new env% [outer env]))
        (send env defvar name init)))

    (define/private (visit-id a)
      (send env get (token-value a)))

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

    (define/private (visit-anonymous-fun a)
      (Function (expr:anonymous-fun-name a)
                   (expr:anonymous-fun-parameters a)
                   (expr:anonymous-fun-body a)
                   env
                   'anonymous
                  nil))

    (define/private (visit-fun a)
      (let ([name (stmt:fun-name a)]
            [pars (stmt:fun-parameters a)]
            [body (stmt:fun-body a)]
            [klass nil])
        (when (~nil? cur-fun)
          (set! klass (Function-klass cur-fun)))
        (send env defvar name (Function name pars body env 'fun klass))))

    (define/private (visit-class a)
      (let ([name (token-value (node-token a))]
            [methods (make-hash)]
            [super-class nil]
            [super-class-name nil]
            [klass nil])
        (unless (nil? (stmt:class-super-class a))
          (set! super-class-name (token-value (stmt:class-super-class a)))
          
          (when (eq? name super-class-name)
            (runtime-error "A class cannot inherit from itself."))
          
          (set! super-class (send env get super-class-name))
          (unless (Class? super-class)
            (runtime-error "Superclass must be a class.")))

        (set! klass (Class name super-class methods (stmt:class-instantiable a)))
        ; create methods
        (for ([i (stmt:class-methods a)])
          (let* ([_name (stmt:fun-name i)]
                 [pars (stmt:fun-parameters i)]
                 [body (stmt:fun-body i)]
                 [fn   (Function _name pars body env 'method klass)])
            (when (eq? _name 'init)
              (set! fn (Function _name pars body env 'init klass)))
            (hash-set! methods _name fn)))
        (send env defvar name klass)))

    (define/private (visit-return a)
      (let ([exp (stmt:return-expr a)])
        (when (~nil? exp)
          (if (initializer? cur-fun)
              (runtime-error "Cannot return a value from an initializer.")
              (set! exp (_eval exp))))
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
            [(expr:list? a)   (visit-list a)]
            [(expr:subscript? a) (visit-subscript a)]
            [(expr:sub-set? a) (visit-sub-set a)]
            [(expr:anonymous-fun? a) (visit-anonymous-fun a)]
            [(stmt:stmts? a)  (visit-stmts a)]
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
