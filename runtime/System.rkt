#lang racket

(require "../types.rkt")

(provide System)

(define System
  (Class
    'System
    nil
    (make-hash
    `([clock . ,(Native 'clock 0 (λ () (/ (current-milliseconds) 1000.0)))]
      [print . ,(Native 'print 1 (lambda (val) (displayln (tostr val))))]))
    #f))