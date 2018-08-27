#lang racket
(require txexpr)

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
; list? procedure? #:keep-where procedure? -> list?
; Somewhat similar to the split procedure for strings. Takes a list
; and returns a list of the same elements of lst, in the same order,
; but placed in sublists. Each sublist ends where an element occurs
; that causes (split-pred? element current-split tail) to be true. The
; next sublist picks up from there. If a split should be empty (such
; as when there are two consecutive elements that cause split-pred? to
; be true), then those splits are not kept.
; The split-map option is supplied because the output of split-where
; may not be a list of splits if the #:keep-where function returns
; 'separate. In this case, the split element is placed on it's own in
; the list of splits. Mapping over the splits (and only the splits) is
; a common enough use-case, I think, that the optional parameter is
; warranted.
; The reason why the objects at which we split are not placed in the
; list as splits is that this function takes after split-string, and
; functions like it from other languages. The thing upon which we
; split is normally removed. Not considered. There are use cases where
; you wouldn't want to throw away that which you split upon, but you'd
; want to run a function over everything else.
(define (split-where lst split-pred? 
          #:keep-where [keep-pred? (λ _ #f)]
          #:split-map [split-func #f])
  (define (iter current-split splits remaining)
    (cond 
      [(null? remaining) 
       (cond
         [(null? current-split) splits]
         [split-func (cons (split-func (reverse current-split)) splits)]
         [else (cons (reverse current-split) splits)])]
      [else
        (match-let
          [((cons elem tail) remaining)]
          (if (split-pred? elem current-split tail)
            (let* 
              [(decision (keep-pred? elem current-split
                                     tail))
               (new-current-split
                 (case decision
                   [(next) (list elem)]
                   [else null]))
               (final-current-split-contents
                 (reverse
                   (case decision
                     [(current) (cons elem current-split)]
                     [else current-split])))
               (processed-current-split
                 (cond
                   [(null? final-current-split-contents)
                    final-current-split-contents]
                   [split-func
                    (split-func final-current-split-contents)]
                   [else final-current-split-contents]))
               (new-splits
                 (case decision
                   [('separate #t)
                    (if (null? processed-current-split)
                      (cons elem splits)
                      (append (list elem processed-current-split)
                              splits))]
                   [else
                     (if (null? processed-current-split)
                       splits
                       (cons processed-current-split splits))]))]
              (iter new-current-split
                    new-splits
                    tail))
            (iter (cons elem current-split)
                  splits
                  tail)))]))
  (reverse (iter null null lst)))

(provide (all-defined-out))
