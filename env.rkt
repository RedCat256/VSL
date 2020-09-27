#lang racket
(require "types.rkt")

(provide env%)

(define env%
  (class object%
    (init-field outer)
    (super-new)

    (field [symtab (make-hash)])

    (define/public (defvar name value)
      (when (and (not (nil? outer)) (has-declared? name))
        (runtime-error (format "Variable '~a' has already declared in this scope." name)))
      (hash-set! symtab name value))

    (define/public (getvar name)
      (hash-ref symtab name))

    (define/public (has-declared? name)
      (hash-has-key? symtab name))

    (define/private (_get name)
      (define cur this)
      (call/cc
       (λ (return)
         (while (not (nil? cur))
           (if (send cur has-declared? name)
               (return cur)
               (set! cur (get-field outer cur))))
         (runtime-error (format "Undefined variable '~a'." name)))))

    (define/public (assign name value)
      (let ([e (_get name)])
        (hash-set! (get-field symtab e) name value)))
          
    (define/public (get name)
      (let ([e (_get name)])
        (send e getvar name)))))
