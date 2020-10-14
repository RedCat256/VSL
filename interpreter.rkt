#lang racket
(require "types.rkt")
(require "util.rkt")
(require "env.rkt")
(require "runtime/system.rkt")

(provide interpreter%)

(define runtime-natives
  (list system-natives))

(define interpreter%
  (class object%
    (init-field in-repl)
    (super-new)

    (define cur-fun nil) ; current activation record
    (define env (new env% [outer nil]))
    
    (define binary-ops `((- ,-) (* ,*) (/ ,divide) (< ,<) (<= ,<=) (> ,>) (>= ,>=)))

    (for ([natives runtime-natives])
      (for ([_ natives])
        (send env defvar (Native-name _) _)))

    (define/private (symbol-function sym)
      (let ([p (assoc sym binary-ops)])
        (if p (second p) #f)))

    (define/private (plus a b)
      (cond [(and (number? a) (number? b)) (+ a b)]
            [(and (string? a) (string? b)) (string-append a b)]
            [else (runtime-error "Operands of '+' must be two numbers or two strings.")]))

    (define/private (visit-unary ast)
      (case (empty-token-type (node-token ast))
        [(-) (let ([v (evaluate (expr:unary-expr ast))])
               (if (number? v)
                   (- v)
                   (runtime-error "Operand of '-' must be number.")))]
        [(!) (falsy? (evaluate (expr:unary-expr ast)))]
        [(|(|) (evaluate (expr:unary-expr ast))]))

    (define/private (_and left right)
      (let ([val (evaluate left)])
        (if (truthy? val)
            (evaluate right)
            val)))

    (define/private (_or left right)
      (let ([val (evaluate left)])
        (if (falsy? val)
            (evaluate right)
            val)))
          
    (define/private (visit-binary ast)
      (let ([type  (empty-token-type (node-token ast))]
            [left  (expr:binary-left ast)]
            [right (expr:binary-right ast)])
        (cond [(eq? type 'and) (_and left right)]
              [(eq? type 'or)  (_or  left right)]
              [else
               (let ([lval  (evaluate left)] [rval  (evaluate right)])
                 (case type
                   [(- * / < <= > >=)
                    (unless (and (number? lval) (number? rval))
                      (runtime-error "Operands of '~a' must be two numbers." type))
                    ((symbol-function type) lval rval)]
                   [(==) (equal? lval rval)]
                   [(!=) (not (equal? lval rval))]
                   [(+)  (plus lval rval)]))])))

    (define/private (visit-block ast)
      (let ([prev env])
        (set! env (new env% [outer env]))
        (with-handlers
            ([runtime-exn? (λ (e) (set! env prev) (raise e))])
          (for ([stmt (stmt:block-slist ast)])
            (evaluate stmt))
          (set! env prev))))

    (define/private (call/fn fn args)
      (let ([env_  (new env% [outer (Function-env fn)])]
            [last  env]
            [arity (length (Function-parameters fn))]
            [return-val nil]
            [prev-fun cur-fun])
        (when (~= arity (length args))
          (runtime-error "Expected ~a arguments but got ~a." arity (length args)))
        (for ([i (Function-parameters fn)] [j args])
          (send env_ defvar i j))
        (set! env env_)
        (set! cur-fun fn)
        (with-handlers
            ([return-exn? (λ (e) (set! return-val (cadr e)))])
          (visit-block (Function-body fn)))
        (when (initializer? fn)
          (set! return-val (send env get 'this)))
        (set! env last)
        (set! cur-fun prev-fun)
        return-val))

    (define/private (call/init cls args)
      (let* ([init #f]
             [name (format "~a instance" (Class-name cls))]
             [ins  (Instance name  cls (make-hash))])
        (cond [(hash-has-key? (Class-methods cls) 'init)
               (set! init (bind/this ins (hash-ref (Class-methods cls) 'init)))
               (call/fn init args)]
              [(positive? (length args)) (runtime-error "Expected ~a arguments but got ~a." 0 (length args))])
        ins))

    (define/private (call/native callee args)
      (if (~= (Native-arity callee) (length args))
          (runtime-error "Expected ~a arguments but got ~a." (Native-arity callee) (length args))
          (apply (Native-fn callee) args)))
      
    (define/private (visit-call ast)
      (let ([callee (evaluate (expr:call-callee ast))]
            [args   (for/list ([_ (expr:call-args ast)]) (evaluate _))])
        (cond [(Function? callee) (call/fn callee args)]
              [(Class? callee) (call/init callee args)]
              [(Native? callee) (call/native callee args)]
              [else (runtime-error "Can only call functions and classes.")])))

    (define/private (bind/this ins fn)
      (let ([env (new env% [outer (Function-env fn)])])
        (send env defvar 'this ins)
        (struct-copy Function fn [env env])))
        
    (define/private (visit-get ast)
      (let ([receiver (evaluate (expr:get-receiver ast))]
            [field    (token-value (node-token ast))]
            [fn #f])
        (unless (Instance? receiver)
          (runtime-error "Only instances have properties."))
        (cond [(instance-has? receiver field) (instance-get receiver field)]
              [(begin (set! fn (class-get receiver field)) fn) (bind/this receiver fn)]
              [else (runtime-error (format "Undefined Property '~a'." field))])))

    (define/private (visit-set ast)
      (let ([receiver (evaluate (expr:set-receiver ast))]
            [field    (token-value (node-token ast))]
            [value    (evaluate (expr:set-expr ast))])
        (unless (Instance? receiver)
          (runtime-error "Only instances have fields."))
        (instance-set receiver field value)
        value))

    (define/private (visit-this ast)
      (send env get 'this))

    (define/private (visit-super ast)
      (let* ([name (token-value (node-token ast))]
             [this (send env get 'this)]
             [super-class (Class-super-class (Function-cls cur-fun))])
        (when (nil? super-class)
          (runtime-error "Cannot use 'super' in a class without superclass."))
        (let ([method (class-get super-class name)])
          (unless method
            (runtime-error "Undefined property '~a'." name))
          (set! method (bind/this this method))
          method)))

    (define/private (visit-list ast)
      (let ([lst #f])
        (set! lst
              (for/list ([e (expr:list-elements ast)])
                (evaluate e)))
        (List lst (length lst))))

    (define/private (check-subscript target index)
      (unless (List? target)
        (runtime-error "Can only apply '[' to a list"))
      (unless (and (exact? index) (integer? index))
        (runtime-error "Subscript must be an integer."))
      (when (or (>= index (List-length target)) (negative? index))
        (runtime-error "Index out of range, expect 0..~a, but got '~a'" (sub1 (List-length target)) index)))

    (define/private (visit-subscript ast)
      (let ([target (evaluate (expr:subscript-target ast))]
            [index  (evaluate (expr:subscript-index ast))])
        (check-subscript target index)
        (list-ref (List-elements target) index)))

    (define/private (visit-sub-set ast)
      (let ([target (evaluate (expr:sub-set-target ast))]
            [index  (evaluate (expr:sub-set-index ast))]
            [value  (evaluate (expr:sub-set-expr ast))])
        (check-subscript target index)
        (let ([v #f])
          (set! v (list->vector (List-elements target)))
          (vector-set! v index value)
          (set-List-elements! target (vector->list v))
          target)))

    (define/private (visit-assign ast)
      (let ([name  (token-value (node-token (node-token ast)))]
            [depth (expr:id-depth (node-token ast))]
            [value (evaluate (expr:assign-expr ast))])
        (send (send env ancestor depth) defvar name value)
        ;(send env assign name value)
        value))

    (define/private (visit-stmts ast)
      (let ([r (void)])
        (for ([stmt (stmt:stmts-slist ast)])
          (set! r (evaluate stmt)))
        (when in-repl (unless (void? r) (tostr r)))))
    
    (define/private (visit-var ast)
      (let ([name (token-value (node-token ast))]
            [init (stmt:var-init ast)])
        (unless (nil? init)
          (set! init (evaluate init)))
        ;persitent environments [crafting interpreters/resolving-and-binding]
        ;(set! env (new env% [outer env]))
        (send env defvar name init)))

    (define/private (visit-id ast)
      (let ([depth (expr:id-depth ast)])
        (send (send env ancestor depth) get (token-value (node-token ast)))))

    (define/private (visit-if ast)
      (let ([test (stmt:if-test ast)]
            [if-arm (stmt:if-if-arm ast)]
            [then-arm (stmt:if-then-arm ast)])
        (if (truthy? (evaluate test))
            (evaluate if-arm)
            (when then-arm
              (evaluate then-arm)))))

    (define/private (visit-while ast)
      (let ([test (stmt:while-test ast)]
            [body (stmt:while-body ast)])
        (with-handlers ([break-exn? (λ (e) nil)])
          (while (truthy? (evaluate test))
            (evaluate body)))))

    (define/private (visit-for ast)
      (let ([init      (stmt:for-init ast)]
            [test (stmt:for-test ast)]
            [increment (stmt:for-increment ast)]
            [body      (stmt:for-body ast)])
        (when init (evaluate init))
        (with-handlers ([break-exn? (λ (e) nil)])
          (while (if test (truthy? (evaluate test)) #t)
            (evaluate body)
            (when increment (evaluate increment))))))

    (define/private (visit-anonymous-fun ast)
      (Function (expr:anonymous-fun-name ast)
                (expr:anonymous-fun-parameters ast)
                (expr:anonymous-fun-body ast)
                env
                'anonymous
                nil))

    (define/private (visit-fun ast)
      (let ([name (stmt:fun-name ast)]
            [pars (stmt:fun-parameters ast)]
            [body (stmt:fun-body ast)]
            [cls nil])
        (when (~nil? cur-fun)
          (set! cls (Function-cls cur-fun)))
        (send env defvar name (Function name pars body env 'fun cls))))

    (define/private (visit-class ast)
      (let ([name (token-value (node-token ast))]
            [super-class nil]
            [super-class-name nil]
            [cls nil])
        (unless (nil? (stmt:class-super-class ast))
          (set! super-class-name (token-value (stmt:class-super-class ast)))
          
          (when (eq? name super-class-name)
            (runtime-error "A class cannot inherit from itself."))
          
          (set! super-class (send env get super-class-name))
          (unless (Class? super-class)
            (runtime-error "Superclass must be a class.")))

        (set! cls (Class name super-class (make-hash)))
        ; create methods
        (for ([i (stmt:class-methods ast)])
          (let* ([name_ (stmt:fun-name i)]
                 [pars (stmt:fun-parameters i)]
                 [body (stmt:fun-body i)]
                 [fn   (Function name_ pars body env 'method cls)])
            (when (eq? name_ 'init)
              (set! fn (Function name_ pars body env 'init cls)))
            (hash-set! (Class-methods cls) name_ fn)))
        (send env defvar name cls)))

    (define/private (visit-return ast)
      (let ([exp (stmt:return-expr ast)])
        (when (~nil? exp)
          (if (initializer? cur-fun)
              (runtime-error "Cannot return a value from an initializer.")
              (set! exp (evaluate exp))))
        (raise `(return ,exp))))

    (define/private (visit-break ast)
      (raise (break-exn)))

    (define/public (evaluate ast)
      (cond [(expr:id? ast)            (visit-id ast)]
            [(expr:unary? ast)         (visit-unary ast)]
            [(expr:binary? ast)        (visit-binary ast)]
            [(expr:call? ast)          (visit-call ast)]
            [(expr:assign? ast)        (visit-assign ast)]
            [(expr:get? ast)           (visit-get ast)]
            [(expr:set? ast)           (visit-set ast)]
            [(expr:this? ast)          (visit-this ast)]
            [(expr:super? ast)         (visit-super ast)]
            [(expr:list? ast)          (visit-list ast)]
            [(expr:subscript? ast)     (visit-subscript ast)]
            [(expr:sub-set? ast)       (visit-sub-set ast)]
            [(expr:anonymous-fun? ast) (visit-anonymous-fun ast)]
            [(stmt:stmts? ast)         (visit-stmts ast)]
            [(stmt:expr? ast)          (evaluate (stmt:expr-expr ast))]
            [(stmt:var? ast)           (visit-var ast)]
            [(stmt:block? ast)         (visit-block ast)]
            [(stmt:if? ast)            (visit-if ast)]
            [(stmt:while? ast)         (visit-while ast)]
            [(stmt:for? ast)           (visit-for ast)]
            [(stmt:fun? ast)           (visit-fun ast)]
            [(stmt:return? ast)        (visit-return ast)]
            [(stmt:break? ast)         (visit-break ast)]
            [(stmt:class? ast)         (visit-class ast)]
            [else (case (empty-token-type ast)
                    [(number string) (token-value ast)]
                    [(true)  #t]
                    [(false) #f]
                    [(nil)   nil]
                    [else    (void)])]))))
