#lang racket
(require "types.rkt")

(provide env%)

(define env%
  (class object%
    (init-field outer)
    (super-new)

    (field [symtab (make-hash)])

    (define/public (defvar name value)
      (when (and (~nil? outer) (contains? name))
        (runtime-error "Variable '~a' has already declared in this scope." name))
      (hash-set! symtab name value))

    (define/public (contains? name)
      (hash-has-key? symtab name))

    (define/private (_get name)
      (define cur this)
      (call/cc
       (λ (return)
         (while (~nil? cur)
           (if (send cur contains? name)
               (return cur)
               (set! cur (get-field outer cur))))
         (runtime-error "Undefined variable '~a'." name))))

    (define/public (assign name value)
      (let ([e (_get name)])
        (hash-set! (get-field symtab e) name value)))
          
    (define/public (get name)
      (let ([e (_get name)])
        (hash-ref (get-field symtab e) name)))))
