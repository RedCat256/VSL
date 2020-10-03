#lang racket

(provide readline)

(require (prefix-in readline: readline/readline))

(define history-file-loaded #f)
(define HISTORY-FILE (format "~a/.vsl.history" (find-system-path 'home-dir)))

(define (load-history path)
  (with-handlers ([exn:fail? (lambda (e) #t)]) 
    (with-input-from-file path
      (lambda ()
        (do ((line (read-line) (read-line)))
          ((eq? line eof))
          (readline:add-history line))))))

(define (readline prompt)
  (unless history-file-loaded
    (load-history HISTORY-FILE)
    (set! history-file-loaded #t))
  (with-handlers
      ([exn:break? (lambda (exn) #f)])
    (let ((line (readline:readline prompt)))
      (if (eq? line eof)
          #f
          (begin (readline:add-history line)
                 (with-handlers ([exn:fail? (lambda (e) #t)])
                   (with-output-to-file HISTORY-FILE
                     (lambda () (printf "~a~n" line))
                     #:exists 'append))
                 line)))))
