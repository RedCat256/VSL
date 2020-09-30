#lang racket

(require "../types.rkt")

(provide system-natives)

(define system-natives
  (list
   (loxNative 'clock 0 (λ () (/ (current-milliseconds) 1000.0)))
   ))