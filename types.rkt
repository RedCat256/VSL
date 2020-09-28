#lang racket

(provide (all-defined-out))

(define nil%
  (class object%
    (super-new)))

(define nil (new nil%))
(define (nil? obj)
  (eq? obj nil))

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

(struct stat:stats   node [slist])
(struct stat:print   node [expr])
(struct stat:expr    node [expr])
(struct stat:var     node [init])
(struct stat:block   node [slist])
(struct stat:if      node [condition if-arm then-arm])
(struct stat:while   node [condition body])
(struct stat:for     node [init condition step body])
(struct stat:fun     node [name parameters body])
(struct stat:return  node [expr])
(struct stat:class   node [methods])

(struct function       [name parameters body env])
(struct loxClass       [name methods])
(struct loxInstance    [name klass fields])

(struct lex-exn     exn:fail:user ())
(struct parse-exn   exn:fail:user ())
(struct runtime-exn exn:fail:user ())

(define-syntax while
  (syntax-rules ()
    ((_ predicate e ...)
     (let loop ()
       (when predicate e ...
         (loop))))))

(define (divide a b)
  (cond [(zero? b) ((if (< a 0) - +) +inf.0)]
        [else (/ a b)]))

(define (char-to-symbol c)
  (string->symbol (string c)))

(define (return-exn? l)
  (and (list? l) (eq? (car l) 'return)))

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

(define (class-has? obj field)
  (when (loxInstance? obj)
    (set! obj (loxInstance-klass obj)))
  (hash-has-key? (loxClass-methods obj) field))

(define (class-get obj field)
  (when (loxInstance? obj)
    (set! obj (loxInstance-klass obj)))
  (hash-ref (loxClass-methods obj) field))