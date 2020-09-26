#lang racket

(provide nil nil? empty-token token lex-exn while
         empty-token-type lex-exn? parse-exn parse-exn?
         token-value expr:unary expr:binary node-token
         expr:binary? expr:unary? expr:unary-expr
         expr:binary-left expr:binary-right)

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

(struct lex-exn exn:fail:user ())
(struct parse-exn exn:fail:user ())

(define-syntax while
  (syntax-rules ()
    ((while predicate e ...)
     (let loop ()
       (when predicate
         e ...
         (loop))))))
