#lang racket

(provide nil nil? empty-token token token? lex-exn while
         empty-token-type lex-exn? parse-exn parse-exn?
         token-value expr:unary expr:binary node-token
         expr:binary? expr:unary? expr:unary-expr
         expr:binary-left expr:binary-right stat:statements
         expr:call expr:call? expr:call-callee expr:call-args
         stat:statements? stat:statements-slist empty-token-line
         stat:print stat:print? stat:print-expr
         stat:expr stat:expr? stat:expr-expr
         stat:var stat:var? stat:var-init
         stat:block stat:block? stat:block-slist
         stat:if stat:if? stat:if-condition stat:if-if-arm stat:if-then-arm
         stat:while stat:while? stat:while-condition stat:while-body
         stat:for stat:for? stat:for-init stat:for-condition stat:for-step stat:for-body
         stat:fun stat:fun? stat:fun-name stat:fun-parameters stat:fun-body
         stat:return stat:return? stat:return-expr return-exn?
         function function? function-name function-parameters function-body function-env
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
(struct expr:call node [callee args])

(struct stat:statements node [slist] #:transparent)
(struct stat:print node [expr] #:transparent)
(struct stat:expr node [expr] #:transparent)
(struct stat:var node [init] #:transparent)
(struct stat:block node [slist] #:transparent)
(struct stat:if node [condition if-arm then-arm])
(struct stat:while node [condition body])
(struct stat:for node [init condition step body])
(struct stat:fun node [name parameters body])
(struct stat:return node [expr])

(struct function [name parameters body env])

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

(define (return-exn? l)
  (and (list? l) (eq? (car l) 'return)))
