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

(struct stmt:stmts   node [slist])
(struct stmt:print   node [expr])
(struct stmt:expr    node [expr])
(struct stmt:var     node [init])
(struct stmt:block   node [slist])
(struct stmt:if      node [condition if-arm then-arm])
(struct stmt:while   node [condition body])
(struct stmt:for     node [init condition increment body])
(struct stmt:fun     node [name parameters body])
(struct stmt:return  node [expr])
(struct stmt:break   node [])
(struct stmt:class   node [super-class methods])

(struct loxFunction    [name parameters body env type klass])
(struct loxClass       [name super-class methods])
(struct loxInstance    [name klass fields])

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
  (eq? (loxFunction-type fn) 'init))

(define (~= a b)
  (not (= a b)))

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
  (hash-has-key? (loxInstance-fields obj) field))

(define (instance-get obj field)
  (hash-ref (loxInstance-fields obj) field))

(define (instance-set obj field value)
  (hash-set! (loxInstance-fields obj) field value))

(define (class-get obj field)
  (let ([klass obj]
        [fn #f])
    (when (loxInstance? obj)
      (set! klass (loxInstance-klass obj)))

    (call/cc
     (λ (return)
       (while (~nil? klass)
         (set! fn (hash-ref (loxClass-methods klass) field #f))
         (when fn
           (return fn))
         (set! klass (loxClass-super-class klass)))
       #f))))
