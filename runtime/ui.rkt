#lang racket
(require htdp/gui)
(require "../types.rkt")

(provide ui-natives)

(define _ Native)

(define ui-natives
  (list
   (_ 'Window  1 (lambda (widgets) (create-window (list (loxList-elements widgets)))))
   (_ 'hideWindow 1 hide-window)
   (_ 'Message 1 make-message)
   (_ 'drawMessage 2 (lambda (m s) (draw-message m s)))
   ))
