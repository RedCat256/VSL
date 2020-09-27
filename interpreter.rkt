#lang racket
(require "types.rkt")
(require "env.rkt")

(provide interpreter%)

(define interpreter%
  (class object%
    (super-new)

    (define env (new env% [outer nil]))

    (define binary-ops (list `(+ ,+) `(- ,-) `(* ,*) `(/ ,/) `(< ,<) `(<= , <=)
                             `(> ,>) `(>= ,>=) `(== ,=) `(!= ,(λ (x y) (not (= x y))))))

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

    (define/public (check p val msg)
      (if (p val) val (runtime-error msg)))

    (define/public (divide a b)
      (cond [(zero? b) ((if (< a 0) - +) +inf.0)]
            [else (/ a b)]))

    (define/public (eval-binary a)
      (define tok (node-token a))
      (define left (expr:binary-left a))
      (define right (expr:binary-right a))
      (define type (empty-token-type tok))
      (case type
        [(+ - * / < <= > >= == !=)
         (let* ([msg (format "Operands of '~a' must be number." type)]
                [lval (check number? (_eval left) msg)]
                [rval (check number? (_eval right) msg)])
           (cond [(eq? type '/) (divide lval rval)]
                 [else ((symbol-function type) lval rval)]))]
        [(and) (and (_eval left) (_eval right))]
        [(or) (or (_eval left) (_eval right))]
        [(=) (eval-assign a)]))

    (define/public (call fn args)
      (let ([_env (new env% [outer (function-env fn)])]
            [last env])
        (for ([i (function-parameters fn)]
              [j args])
          (send _env defvar i j))
        (set! env _env)
        (with-handlers
            ([return-exn? (λ (e) (begin0 (cadr e) (set! env last)))])
          (eval-block (function-body fn))
          (set! env last))))
      
    (define/public (eval-call a)
      (define callee (_eval (expr:call-callee a)))
      (define num_a (length (expr:call-args a)))
      (define num_p 0)
      (unless (function? callee)
        (runtime-error "Expect callable object before '('."))
      (set! num_p (length (function-parameters callee)))

      (when (not (= num_a num_p))
        (runtime-error (format "Expect ~a arguments, got ~a." num_p num_a)))
      
      (call callee (for/list ([arg (expr:call-args a)]) (_eval arg))))
    
    (define/public (eval-assign a)
      (let* ([left (expr:binary-left a)]
             [name (token-value left)]
             [right (expr:binary-right a)]
             [value (_eval right)])
        (send env assign name value)
        value))

    (define/public (eval-statements a)
      (let ([r (void)])
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
      (with-handlers
          ([runtime-exn? (λ (e) (eprintf "RuntimeError: ~a~n" (exn-message e)) (set! env previous))])
        (for ([stat (stat:block-slist a)])
          (_eval stat))
        (set! env previous)))

    (define/public (eval-if a)
      (let ([condition (stat:if-condition a)]
            [if-arm (stat:if-if-arm a)]
            [then-arm (stat:if-then-arm a)])
        (if (_eval condition)
            (_eval if-arm)
            (when then-arm
              (_eval then-arm)))))

    (define/public (eval-while a)
      (let ([condition (stat:while-condition a)]
            [body (stat:while-body a)])
        (while (_eval condition)
          (_eval body))))

    (define/public (eval-for a)
      (let ([init (stat:for-init a)]
            [condition (stat:for-condition a)]
            [step (stat:for-step a)]
            [body (stat:for-body a)])
        (when init (_eval init))
        (while (_eval condition)
          (_eval body)
          (_eval step))))

    (define/public (eval-fun a)
      (let ([name (stat:fun-name a)]
            [pars (stat:fun-parameters a)]
            [body (stat:fun-body a)])
        (send env defvar name (function name pars body (send env copy-env)))))

    (define/public (eval-return a)
      (let ([val (_eval (stat:return-expr a))])
        (raise `(return ,val))))

    (define/public (_eval a)
      (cond [(expr:unary? a) (eval-unary a)]
            [(expr:binary? a) (eval-binary a)]
            [(expr:call? a) (eval-call a)]
            [(stat:statements? a) (eval-statements a)]
            [(stat:print? a) (eval-print a)]
            [(stat:expr? a) (_eval (stat:expr-expr a))]
            [(stat:var? a) (eval-varDecl a)]
            [(stat:block? a) (eval-block a)]
            [(stat:if? a) (eval-if a)]
            [(stat:while? a) (eval-while a)]
            [(stat:for? a) (eval-for a)]
            [(stat:fun? a) (eval-fun a)]
            [(stat:return? a) (eval-return a)]
            [else (case (empty-token-type a)
                    [(number string) (eval-literal a)]
                    [(true) #t]
                    [(false) #f]
                    [(nil) nil]
                    [(id) (eval-id a)]
                    [else (void)])]))))
