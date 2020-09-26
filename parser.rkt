#lang racket

(require "types.rkt")

(provide parser%)

(define parser%
  (class object%
    (init tokens)
    (super-new)

    (define toks tokens)
    (define pos 0)
    (define cur (peek))
    (define prev #f)
    
    (define/public (peek)
      (if (>= pos (length toks))
          nil
          (list-ref toks pos)))

    (define/public (next)
      (set! prev cur)
      (set! pos (add1 pos))
      (set! cur (peek)))

    (define/public (parse-error msg)
      (raise (parse-exn msg (current-continuation-marks))))

    (define/public (consume type msg)
      (if (check type)
          (next)
          (parse-error msg)))

    (define/public (check type)
      (eq? (empty-token-type cur) type))

    (define/public (_match . types)
      (call/cc
       (λ (return)
         (for ([type types])
           (when (eq? type (empty-token-type cur))
             (next)
             (return #t)))
         #f)))

    (define/public (not-at-end?)
      (not (check 'eof)))

    (define/public (synchronize)
      (next)
      (call/cc
       (lambda (return)
         (while (not-at-end?)
           (when (eq? (empty-token-type prev) '|;|)
             (return nil))
           (case (empty-token-type cur)
             [(class fun var for if while print return) (return nil)])
           (next)))))

    (define/public (parse-prec prec)
      (define left (unary))
      (while (< prec (get-prec cur))
        (next)
        (set! left (binary left)))
      left)

    (define/public (unary)
      (define type (empty-token-type cur))
      (case type
        [(+) (next) (parse-prec 120)]
        [(-) (next) (- (parse-prec 120))]
        [(!) (next) (not (parse-prec 120))]
        [(number) (next) (token-value prev)]
        [else (parse-error (format "Invalid unary operator '~a'" type))]))

    (define/public (binary left)
      (define type (empty-token-type prev))
      (case type
        [(+) (+ left (parse-prec 100))]
        [(-) (- left (parse-prec 100))]
        [(*) (* left (parse-prec 110))]
        [(/) (/ left (parse-prec 110))]
        [else (parse-error (format "Invalid binary operator '~a'" type))]))
       
    (define/public (get-prec tok)
      (case (empty-token-type tok)
        [(+ -) 100]
        [(* /) 110]
        [else 0]))

    (define/public (expr)
      (parse-prec 0))))
