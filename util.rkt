#lang racket

(provide (all-defined-out))

(define-syntax while
  (syntax-rules ()
    ((_ predicate e ...)
     (let loop ()
       (when predicate e ...
         (loop))))))

(define-syntax until
  (syntax-rules ()
    ((_ predicate e ...) (while (not predicate) e ...))))

(define-syntax incf
  (syntax-rules ()
    ((_ x n) (set! x (+ x n)))
    ((_ x) (incf x 1))))

(define (divide a b)
  (cond [(zero? b) (/ (exact->inexact a) (exact->inexact b))]
        [else (/ a b)]))


(define (stringify a)
  (let* ([s (number->string a)]
         [n (string-length s)])
    (if (and (> n 2) (string=? (substring s (- n 2) n) ".0"))
        (substring s 0 (- n 2))
        s)))
