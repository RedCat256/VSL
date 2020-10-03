#lang racket

(provide (all-defined-out))

(define nil%
  (class object%
    (super-new)))

(define nil (new nil%))
(define (nil? obj) (eq? obj nil))
(define (~nil? obj) (not (eq? obj nil)))

(struct empty-token [type line])
(struct token empty-token [value])

(struct node [token])

(struct expr:unary   node [expr])
(struct expr:binary  node [left right])
(struct expr:assign  node [expr])
(struct expr:call    node [callee args])
(struct expr:get     node [receiver])
(struct expr:set     node [receiver expr])
(struct expr:this    node [])
(struct expr:super   node [])
(struct expr:list    node [elements])
(struct expr:subscript node [target index])
(struct expr:sub-set node [target index expr])
(struct expr:anonymous-fun node [name parameters body])

(struct stmt:stmts   node [slist])
(struct stmt:expr    node [expr])
(struct stmt:var     node [init])
(struct stmt:block   node [slist])
(struct stmt:if      node [condition if-arm then-arm])
(struct stmt:while   node [condition body])
(struct stmt:for     node [init condition increment body])
(struct stmt:fun     node [name parameters body])
(struct stmt:return  node [expr])
(struct stmt:break   node [])
(struct stmt:class   node [super-class methods instantiable])

(struct Function    [name parameters body env type klass])
(struct Native      [name arity fn])
(struct Class       [name super-class methods instantiable])
(struct Instance    [name klass fields])
(struct List        [elements length] #:mutable)

(struct lex-exn     exn:fail:user ())
(struct parse-exn   exn:fail:user ())
(struct runtime-exn exn:fail:user ())
(struct break-exn   [])

(define-syntax while
  (syntax-rules ()
    ((_ predicate e ...)
     (let loop ()
       (when predicate e ...
         (loop))))))

(define-syntax incf
  (syntax-rules ()
    ((_ x n) (set! x (+ x n)))
    ((_ x) (incf x 1))))

(define (divide a b)
  (cond [(zero? b) (/ (exact->inexact a) (exact->inexact b))]
        [else (/ a b)]))

(define (plus a b)
  (cond [(and (number? a) (number? b)) (+ a b)]
        [(and (string? a) (string? b)) (string-append a b)]
        [else (runtime-error "Operands of '+' must be two numbers or two strings.")]))

(define (truthy? a)
  (and (~nil? a) a))

(define (falsy? a)
  (not (truthy? a)))

(define (stringify a)
  (let* ([s (number->string a)]
         [n (string-length s)])
    (if (and (> n 2) (string=? (substring s (- n 2) n) ".0"))
        (substring s 0 (- n 2))
        s)))

(define (member? a l)
  (cond [(null? l) #f]
        [(equal? a (car l)) #t]
        [else (member? a (cdr l))]))

(define (char-to-symbol c)
  (string->symbol (string c)))

(define (return-exn? l)
  (and (list? l) (eq? (car l) 'return)))

(define (initializer? fn)
  (eq? (Function-type fn) 'init))

(define (~= a b)
  (not (= a b)))

(define (list-to-str lst)
  (string-join (for/list ([i lst])
                  (tostr i))
                ", "
                #:before-first "["
                #:after-last "]"))

(define (tostr val)
  (cond [(string? val) val]
        [(number? val) (stringify (exact->inexact val))]
        [(eq? #t val)  "true"]
        [(eq? #f val)  "false"]
        [(nil? val)    "nil"]
        [(List? val) (list-to-str (List-elements val))]
        [(Function? val) (format "<fn ~a>" (Function-name val))]
        [(Native? val)   (format "<fn ~a>" (Native-name val))]
        [(Instance? val) (Instance-name val)]
        [(Class? val)    (Class-name val)]
        [else "Unknown data type"]))

(define (mkerr exn)
  (λ x
    (raise (exn (apply format x) (current-continuation-marks)))))

(define-values (lex-error parse-error runtime-error)
  (values (mkerr lex-exn) (mkerr parse-exn) (mkerr runtime-exn)))

(define (user-exn-catched? exn)
  (or (lex-exn? exn) (parse-exn? exn) (runtime-exn? exn)))

(define (print-user-error exn)
  (let ([msg (exn-message exn)])
    (cond [(lex-exn? exn)     (eprintf "LexError: ~a~n" msg)]
          [(parse-exn? exn)   (eprintf "ParseError: ~a~n" msg)]
          [(runtime-exn? exn) (eprintf "RuntimeError: ~a~n" msg)])))

(define (instance-has? obj field)
  (hash-has-key? (Instance-fields obj) field))

(define (instance-get obj field)
  (hash-ref (Instance-fields obj) field))

(define (instance-set obj field value)
  (hash-set! (Instance-fields obj) field value))

(define (class-get obj field)
  (let ([klass obj]
        [fn #f])
    (when (Instance? obj)
      (set! klass (Instance-klass obj)))

    (call/cc
     (λ (return)
       (while (~nil? klass)
         (set! fn (hash-ref (Class-methods klass) field #f))
         (when fn
           (return fn))
         (set! klass (Class-super-class klass)))
       #f))))
