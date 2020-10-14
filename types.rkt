#lang racket
(require "util.rkt")

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
(struct stmt:if      node [test if-arm then-arm])
(struct stmt:while   node [test body])
(struct stmt:for     node [init test increment body])
(struct stmt:fun     node [name parameters body])
(struct stmt:return  node [expr])
(struct stmt:break   node [])
(struct stmt:class   node [super-class methods])

(struct Function    [name parameters body env type cls])
(struct Native      [name arity fn])
(struct Class       [name super-class methods])
(struct Instance    [name cls fields])
(struct List        [elements length] #:mutable)

(struct runtime-exn exn:fail:user ())
(struct break-exn   [])

(define (runtime-error . args)
  (raise (runtime-exn (apply format args) (current-continuation-marks))))


(define (truthy? a)
  (and (~nil? a) a))

(define (falsy? a)
  (not (truthy? a)))

(define (char-to-symbol c)
  (string->symbol (string c)))

(define (return-exn? l)
  (and (list? l) (eq? (car l) 'return)))

(define (initializer? fn)
  (eq? (Function-type fn) 'init))

(define (~= a b)
  (not (= a b)))

(define (tostr val)
  (cond [(string? val) (format "\x1b[1;33m~a~a" val "\x1b[0m")]
        [(number? val) (format "\x1b[1;34m~a~a" (stringify (exact->inexact val)) "\x1b[0m")]
        [(eq? #t val)  "\x1b[1;35mtrue\x1b[0m"]
        [(eq? #f val)  "\x1b[1;35mfalse\x1b[0m"]
        [(nil? val)    "nil"]
        [(List? val) (list-to-str (List-elements val))]
        [(Function? val) (format "\x1b[36m<fn ~a>\x1b[0m" (Function-name val))]
        [(Native? val)   (format "\x1b[36m<fn ~a>\x1b[0m" (Native-name val))]
        [(Instance? val) (format "\x1b[37m~a~a" (Instance-name val) "\x1b[0m")]
        [(Class? val)    (format "\x1b[37m~a~a" (Class-name val) "\x1b[0m")]
        [(void? val) ""]
        [else "Unknown data type"]))

(define (list-to-str lst)
  (string-join (for/list ([i lst])
                 (tostr i))
               ", "
               #:before-first "["
               #:after-last "]"))

(define (instance-has? obj field)
  (hash-has-key? (Instance-fields obj) field))

(define (instance-get obj field)
  (hash-ref (Instance-fields obj) field))

(define (instance-set obj field value)
  (hash-set! (Instance-fields obj) field value))

(define (class-get obj field)
  (let ([cls obj]
        [fn #f])
    (when (Instance? obj)
      (set! cls (Instance-cls obj)))

    (call/cc
     (λ (return)
       (while (~nil? cls)
         (set! fn (hash-ref (Class-methods cls) field #f))
         (when fn
           (return fn))
         (set! cls (Class-super-class cls)))
       #f))))
