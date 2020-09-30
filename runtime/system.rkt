#lang racket

(require "../types.rkt")

(provide system-natives)

(define system-natives (list
    (loxNative 'clock 0 current-milliseconds)
))