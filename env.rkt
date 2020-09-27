#lang racket
(require "types.rkt")

(provide env%)

(define env%
  (class object%
    (init outer)
    (super-new)

    (define _outer outer)
    (define symtab (make-hash))

    (define/public (defvar name value)
      (when (and (not (nil? _outer)) (has-declared? name))
        (runtime-error (format "Variable '~a' has already declared in this scope." name)))
      (hash-set! symtab name value))

    (define/public (getvar name)
      (hash-ref symtab name))

    (define/public (has-declared? name)
      (hash-has-key? symtab name))

    (define/public (_get name)
      (define cur this)
      (call/cc
       (λ (return)
         (while (not (nil? cur))
           (if (send cur has-declared? name)
               (return cur)
               (set! cur (send cur get-outer))))
         (runtime-error (format "Undefined variable '~a'." name)))))

    (define/public (get-outer) _outer)
    
    (define/public (set-outer! x)
      (set! _outer x))
    
    (define/public (set-symtab! x)
      (set! symtab x))

    (define/public (copy-env)
      (define new_env (new env% [outer nil]))
      (unless (nil? _outer)
        (send new_env set-outer! (send _outer copy-env)))
      (send new_env set-symtab!(hash-copy symtab))
      new_env)

    (define/public (assign name value)
      (let ([e (_get name)])
        (send e defvar name value)))
          
    (define/public (get name)
      (let ([e (_get name)])
        (send e getvar name)))))
