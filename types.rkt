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

(struct expr:unary node [expr])
(struct expr:binary node [left right])
(struct expr:call node [callee args])

(struct stat:stats node [slist])
(struct stat:print node [expr])
(struct stat:expr node [expr])
(struct stat:var node [init])
(struct stat:block node [slist])
(struct stat:if node [condition if-arm then-arm])
(struct stat:while node [condition body])
(struct stat:for node [init condition step body])
(struct stat:fun node [name parameters body])
(struct stat:return node [expr])

(struct function [name parameters body env])

(struct lex-exn exn:fail:user ())
(struct parse-exn exn:fail:user ())
(struct runtime-exn exn:fail:user ())

(define-syntax while
  (syntax-rules ()
    ((while predicate e ...)
     (let loop ()
       (when predicate
         e ...
         (loop))))))

(define (return-exn? l)
  (and (list? l) (eq? (car l) 'return)))

(define (make-error exn)
  (λ (msg)
    (raise (parse-exn msg (current-continuation-marks)))))

(define-values (lex-error parse-error runtime-error)
  ((λ (fn)
     (values (fn lex-exn) (fn parse-exn) (fn runtime-exn))) make-error))
   