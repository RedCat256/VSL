#lang racket

(provide nil nil? empty-token token lex-exn while
         empty-token-type lex-exn?)

(define nil%
  (class object%
    (super-new)))

(define nil (new nil%))
(define (nil? obj)
  (eq? obj nil))

(struct empty-token [type line] #:transparent)
(struct token empty-token [value] #:transparent)

(struct lex-exn exn:fail:user ())

(define-syntax while
  (syntax-rules ()
    ((while predicate e ...)
     (let loop ()
       (when predicate
         e ...
         (loop))))))
