#lang racket
(require htdp/gui)
(require "../types.rkt")

(provide ui-natives)

(define _ loxNative)

(define ui-natives
  (list
   (_ 'createWindow 0 (lambda () (create-window null)))
   (_ 'hideWindow 1 hide-window)
   (_ 'Button 2 (lambda (text cb)
                    (make-button text (loxNative-fn cb))))
   ))
