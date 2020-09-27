#lang racket

(provide nil nil? empty-token token lex-exn while
         empty-token-type lex-exn? parse-exn parse-exn?
         token-value expr:unary expr:binary node-token
         expr:binary? expr:unary? expr:unary-expr
         expr:binary-left expr:binary-right stat:statements
         stat:statements? stat:statements-slist empty-token-line
         stat:print stat:print? stat:print-expr
         stat:expr stat:expr? stat:expr-expr
         stat:var stat:var? stat:var-init
         stat:block stat:block? stat:block-slist
         runtime-exn runtime-exn? runtime-error)

(define nil%
  (class object%
    (super-new)))

(define nil (new nil%))
(define (nil? obj)
  (eq? obj nil))

(struct empty-token [type line] #:transparent)
(struct token empty-token [value] #:transparent)

(struct node [token] #:transparent)
(struct expr:unary node [expr] #:transparent)
(struct expr:binary node [left right] #:transparent)
(struct stat:statements node [slist] #:transparent)
(struct stat:print node [expr] #:transparent)
(struct stat:expr node [expr] #:transparent)
(struct stat:var node [init] #:transparent)
(struct stat:block node [slist] #:transparent)

(struct lex-exn exn:fail:user ())
(struct parse-exn exn:fail:user ())
(struct runtime-exn exn:fail:user ())

(define (runtime-error msg)
  (raise (runtime-exn msg (current-continuation-marks))))

(define-syntax while
  (syntax-rules ()
    ((while predicate e ...)
     (let loop ()
       (when predicate
         e ...
         (loop))))))
