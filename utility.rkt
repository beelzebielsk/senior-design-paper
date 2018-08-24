#lang racket

(define (number->bits num)
  (if (< num 2)
    (list num)
    (append (number->bits (quotient num 2)) 
            (list (remainder num 2)))))
(define (enumerate lst)
  (build-list (length lst) identity))
(define printable? (or/c string? number? symbol?))
(define (reverse* val)
  (cond [(null? val) val]
        [(pair? val) (reverse (map reverse* val))]
        [else val]))
(define (my-flatten lst)
  (foldl
    (λ (cur prev)
       (if (pair? cur)
         (append prev (my-flatten cur))
         (append prev (list cur))))
    null lst))
(define (math-flatten xexpr)
  (define (helper xexpr in-math-tag?)
    (displayln xexpr)
    (if (txexpr? xexpr)
      (txexpr 
        (get-tag xexpr)
        (get-attrs xexpr)
        (foldl
          (λ (elem result)
             (if (is-tag? elem 'math)
               (if in-math-tag?
                 (append result (get-elements (helper elem #t)))
                 (append result (list (helper elem #t))))
               (append result (list elem))))
             null (get-elements xexpr)))
        xexpr))
  (helper xexpr #f))
(define (tree-depth val)
  (cond [(null? val) 0]
        [(list? val)
         (add1 (apply max (map tree-depth val)))]
        [else 0]))
(define (add-ns ns) 
  (λ (tx) 
     (txexpr
       (string->symbol (format "~a:~a" ns (get-tag tx)))
       (get-attrs tx)
       (get-elements tx))))
(define (is-tag? tag name)
  (and (txexpr? tag) (eq? (get-tag tag) name)))
(provide (all-defined-out))
