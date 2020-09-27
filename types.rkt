#lang racket

(provide (all-defined-out))

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
