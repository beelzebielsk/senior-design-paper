#lang racket
(define (macro name . args)
  (string-append
    (format "\\~a" (symbol->string name))
    (if (pair? args)
      (string-join 
        args "}{"
        #:before-first "{"
        #:after-last "}")
      "")))
(define (environment name #:args [args null] . body)
  (string-append
    (apply macro 'begin (symbol->string name) args)
    "\n"
    (string-join body " ")
    "\n"
    (macro 'end (symbol->string name))))
;(define (ensure-math text) (macro 'ensuremath text))
(provide (all-defined-out))
