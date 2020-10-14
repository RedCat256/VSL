#lang racket
(require "types.rkt")
(require "util.rkt")

(provide env%)

(define env%
  (class object%
    (init-field outer)
    (super-new)

    (field [symtab (make-hash)])

    (define/public (defvar name value)
      (hash-set! symtab name value))

    (define/private (_get name)
      (define cur this)
      (call/cc
       (λ (return)
         (while (~nil? cur)
           (if (hash-has-key? (get-field symtab cur) name)
               (return cur)
               (set! cur (get-field outer cur))))
         (runtime-error "Undefined variable '~a'." name))))

    (define/public (ancestor depth)
      (let ([r this])
        (while (> depth 0)
          (set! r (get-field outer r))
          (incf depth -1))
        r))
          
    (define/public (get name)
      (let ([e (_get name)])
        (hash-ref (get-field symtab e) name)))))
