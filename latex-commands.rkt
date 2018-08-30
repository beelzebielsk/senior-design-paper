#lang racket
(require txexpr "utility.rkt")
(define (macro-args args)
  (add-between args '("}{")
               #:splice? #t
               #:before-first '("{")
               #:after-last '("}")))
(define (macro name . args)
  (list-splice
    (cons (format "\\~a"  name)
          (if (pair? args) (macro-args args) null))))
; string? name
; #:args
(define (environment name 
                     #:args [args null] 
                     #:before-args [before null]
                     #:after-args [after null] 
                     . body)
  (list-splice
        (macro 'begin (symbol->string name))
        (list-splice before)
        (list-splice (macro-args args))
        (list-splice after)
        "\n"
        (list-splice body)
        "\n"
        (macro 'end (symbol->string name))))
(define (environment-tag environment body)
  (txexpr 'environment `((name ,(~a environment))) body))
(provide (all-defined-out))
