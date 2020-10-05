#lang racket

(require "../types.rkt")

(provide system-natives)

(define system-natives
  (list
   (Native 'clock 0 (λ () (/ (current-milliseconds) 1000.0)))
   (Native 'print 1 (λ (val) (displayln (tostr val))))
   ))