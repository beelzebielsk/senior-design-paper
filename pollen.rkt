#lang racket
(require txexpr 
         "latex-commands.rkt"
         "manual-traverse.rkt"
         "utility.rkt"
         pollen/tag
         pollen/decode
         racket/stxparam)

(define-syntax ddlog
  (lambda (stx)
    (syntax-case stx ()
      [name (identifier? #'name) #'"DDLOG"]
      [(_ args ...)
       #'(ensure-math "DDLOG" args ...)])))
(define-syntax key
  (lambda (stx)
    (syntax-case stx ()
      [name (identifier? #'name) #'(ensure-math (macro 'lambda))]
      [(_ index) #'(bit key index)])))
(define-syntax input
  (lambda (stx)
    (syntax-case stx ()
      [name 
        (identifier? #'name) 
        #'(ensure-math (macro 'mathbb "I"))]
      [(_ args ...) 
       #'(ensure-math input "(" args ... ")")])))

(define-tag-function 
  (math attrs elems)
  (txexpr 'math attrs elems))
(define-tag-function
  (ensure-math attrs elems)
  (txexpr 'ensure-math attrs (report elems)))
;(define ensure-math math)

(define-syntax-rule (define-math-tag (name attrs elems) body ...)
  (define-tag-function 
    (name attrs elems) 
    (let [(result ((lambda (attrs elems) body ...) attrs elems))]
      (ensure-math result)
      #;(if (and (list? result) (not (txexpr? result)))
        (apply ensure-math result)
        (ensure-math result)))))

; The difference between flatten and what I'm going for is that I'm
; not going to flatten everything.
; If I assume that the only tag left are math tags, which means that
; the document now looks like:
;   ('root (or/c string? math-tag?) ...)
; And that math tags may contain either strings or other math-tags,
; then all I'm really trying to do is call a special flatten on all
; the math tags. It's a txexpr-flatten which includes only the
; elements of the expression within the parent expression.
;
; If I wait until the absolute last moment to handle the math nesting,
; then one interesting consequence of this is that all the "top-level"
; math tags will necessarily be children of the root tag. This is
; because if the 'root tag has a tag as a child at this point, the
; only remaining tags at this point are math tags.

; ensure-math should expand to math, unless inside of a math tag.
; Can be accomplished through syntax parameters.
; ensure-math will be syntax parameter which expands to a math tag.
; math tags change that syntax parameter to be something
; inconsequential.
(define (math-process xexpr)
  (cond [(report (is-tag? (report xexpr) 'math))
         (append 
           '(@ "$")
           ;'(@)
           (decode-elements (get-elements xexpr)
                            #:txexpr-proc (lambda (tx) 
                                            (if (is-tag? tx 'ensure-math) 
                                              (get-elements tx)
                                              tx))) '("$"))]
                                              ;tx))))]
        ; We know that this is not contained within a math tag, for if
        ; it were, the above branch would have been taken already, and
        ; decode would've been applied to this tag.
        [(is-tag? xexpr 'ensure-math)
         (math-process (txexpr 'math null (get-elements xexpr)))]
        [(txexpr? xexpr)
         (map (lambda (e) (if (txexpr? e) (math-process e) e)) xexpr)]
        [else xexpr]))

(define-tag-function ($ _ text)
  (apply math text))
(define-tag-function ($$ _ text)
  (apply math #:display "" text))
(define-math-tag (share _ text)
  (list-splice `(,(macro 'langle) " " ,@text " " ,(macro 'rangle))))
(define-math-tag 
  (secret-share _ text)
  (apply share (list key "(" (cons '@ text) ")")))

  ;(apply share (flatten (list key "(" text ")"))))
(define-math-tag 
  (set _ text)
  (list-splice 
    (append (list "\\{")
            (add-between text ", ")
            (list "\\}"))))
(define mult (ensure-math (macro 'otimes)))
(define-math-tag (encryption _ text)
  ;(define content (string-join (map ~a text) " "))
  (list-splice `(,(macro 'llbracket) " " 
                  ,@text " " 
                  ,(macro 'rrbracket) "_{" ,key "}")))
(define-math-tag 
  (server _ elements)
  (let ([index (first elements)])
    (list-splice "S_{" index "}")))
(define-tag-function (congruent attr elem) "")


(define location (ensure-math "M"))
; TODO: Extract the norm-arguments routine here. The routine is about
; having space-separated arguments to a tag. This isn't the only place
; where I'd want that. And I might want to change the delimiter, too.
;(define-math-tag (bit _ text)
(define (bit . text)
  (report text)
  (define (norm-arguments args)
    (cond [(>= (length args) 2)
           (take args 2)]
          [(and (= 1 (length args))
                (string? (first args))
                (string-contains? (first args) " "))
           (norm-arguments (string-split (first args)))]
          [else null]))
  (let [(args (norm-arguments text))]
    (if (null? args)
      (error "Tag `bit` takes two space-separated arguments"
             text)
      (match-let [((list base index) args)]
        (ensure-math (list-splice (report base) "^{(" index ")}"))))))

; - Core tags
;   - macro
;   - environment
;   - math
; - Convenience tags
;   - ol (ordered list)
;       - Initially, it will have a mixture of 'i tags and lines that
;         start with some bullet.
;       - After processing, it's children will all be 'i tags.
;   - l (unordered list)
;       - Same as for ol.
;   - eql (equation list)
;       - Same as for ol.
;   - table
;       - The only children that table can have are rows.
;       - Rows can have plain text or 'cell tags as children. 
;       - After processing, all lines of plain text will become a
;         'cell tag.
; Everything else should trasnform pretty diectly into a rendered
; form. Putting it into a tag is optional.

(define (split-list-at-tag-or-newline lst)
  (split-where 
    lst 
    (λ (current . _) 
       (or (and (string? current) (string=? "\n" current))
           (is-tag? current 'i)))
    #:keep-where 
    (λ (split-elem . _) 
       (if (txexpr? split-elem) 'separate 'ignore))
    #:split-map
    (λ (current . _) 
       (txexpr 'i null current))))
    
(define (eql . text)
  (displayln (format "eql contents: ~v" text))
  (displayln (format "eql split at newlines: ~v" 
                     (split-list-at-tag-or-newline text)))
  ; contextual processing: if a printable? or series of printable?
  ; is/are followed by a newline or a txexpr?, then wrap that in an
  ; 'i tag. The following is done under the assumption that the only
  ; elements in text are txexpr? and printable?. 
  (txexpr 'eql null (split-list-at-tag-or-newline text)))

(define (split-list-at-bullets-or-list-tags lst bullet-pattern list-tag)
  (split-where
    lst
    (λ (elem current-split . _)
       ;(displayln (~v elem))
       ;(displayln (~v current-split))
       (report (~v elem))
       (report (~v current-split))
       (or (is-tag? elem list-tag)
           (and (string? elem) 
                (regexp-match bullet-pattern elem)
                (not (null? current-split))
                ; TODO: There's more I can do here. I might want to
                ; give more control to the user on how to insert a
                ; value into the current split, consume a few extra
                ; values from the remaining, and such. Because I'd
                ; like for split-where to take care of elminating
                ; newlines around the bullets, too.
                ; NOTE: Remember that, before you place the split in
                ; splits, the split is in reverse order. That's a
                ; nasty implementation detail and ought to be changed.
                ; Besides, whatever you're doing with the consing,
                ; it's not saving you time. The Big O of append ought
                ; to be the length of the 1st list, which would be
                ; the... nope I'm wrong. I'm appending onto the end of
                ; the list, so this actually would be quadratic. No
                ; thanks.
                (string=? "\n" (first current-split)))))
    #:keep-where 
    (λ (current . _) 
       (if (string? current)
         'next
         'separate))
    #:split-map
    (λ (current . _)
       (if (txexpr? current)
         current
         (txexpr list-tag
                 null
                 (cons
                   (regexp-replace bullet-pattern (first current) "")
                   (rest current)))))))

(define-tag-function
  (l attrs elems)
  ;(report elems)
  (let* ([splits (report (split-list-at-bullets-or-list-tags 
                           elems 
                           #px"^\\s*-" 
                           'i))]
         [list-item-of-blanks?
           (lambda (txexpr)
             ((listof 
                (lambda (v) 
                  (and (string? v) (regexp-match #px"^\\s*$" v))))
              (get-elements txexpr)))]
         [cleaned (filter-not list-item-of-blanks? splits)])
  (txexpr 'list '((type "unordered")) cleaned)))
(define-tag-function
  (ol attrs elems)
  (txexpr 'list '((type "ordered")) 
          (split-list-at-bullets-or-list-tags elems #px"^\\s*-" 'i)))
(define-tag-function 
  (row attrs elems) 
  (txexpr 'row null (split-list-at-tag-or-newline elems)))


; TODO: Fill this in with something appropriate. Should produce a big
; sigma letter and stuff.
;(define-math-tag (sum attrs text) "")
(define-tag-function 
  (note attrs text)
  (apply environment 'note #:args (list "Note") text))
(define-tag-function 
  (example attrs text)
  (apply environment 'example text))
(define-tag-function
  (h1 attrs text)
  (apply macro 'section text))
(define-tag-function
  (h2 attrs text)
  (apply macro 'subsection text))

(define (todo . _) "")

; This function is about preventing decode-elements from acting on the
; temporarily created root tag for the elements to decode. Pollen
; introduces it as a quick-and-dirty way to implement this function
; using decode. It causes a function that acts on txexpr (eg.
; #:txexpr-proc) to not run on the temporary root tag. It depends on a
; silly implementation detail of Pollen, so this is a temporary fix.
;
; Careful when using decode-elements. It's a little weird. It
; functions by taking your list of elements and wrapping them around
; in a temporary tag, then passing that off to decode. Unfortunately,
; any function that gets called on a tag will get called for this
; temporary tag. 
; To make this concrete, something like:
;   (decode-elements (list ...))
; Becomes
;   (decode ('temp-tag ...))
; The final result of decode-elements is 
;   (get-elements (decode ('temp-tag ...)))              
; So, if you do something like I did, where I return a list of xexpr?
; from the #:txexpr-proc all the time: (a silly example follows)
;   (decode-elements #:txexpr-proc get-elements null)
; results in
;   (get-elements (decode #:txexpr-proc get-elements ('temp-tag)))
; The application of decode rightly results in null, because we do
;   (get-elements ('temp-tag))
; But then we have:
;   (get-elements null)
; Which will fail.
; And even worse, in the following example, decode-elements will have
; strange and unexpected results:
;   (decode-elements #:txexpr get-elements '((i 1) (i 2) (i 3) (i 4)))
; I intended this as a sort of flattening of the tags, but what
; happens is:
;   (decode #:txexpr get-elements ('temp-tag (i 1) (i 2) (i 3) (i 4)))
; Which results in (roughly):
;   (decode #:txexpr get-elements ('temp-tag 1 2 3 4))
; Which results in '(1 2 3 4)
; And 
;   (get-elements '(1 2 3 4))
; Results in
;   '(3 4)
; Because the initial tag and (what is thought to be the attributes)
; are removed from the list, leaving just the 1st two things.
; I also had another strange bug, where I was applying decode-elements
; to an actual txexpr, which caused me to lose only the 1st element:
;   (get-elements 
;       (decode-elements #:txexpr-proc 
;                        get-elements 
;                        '(eql (i 1) (i 2) (i 3))))
;
;   (get-elements 
;       (decode #:txexpr-proc 
;                        get-elements 
;                        '(temp-tag eql (i 1) (i 2) (i 3))))
;
;   (get-elements 
;       (decode #:txexpr-proc 
;                        get-elements 
;                        '(temp-tag eql 1 2 3)))
;
;   (get-elements '(eql 1 2 3)))
;
;   '(2 3)
;
; And thus I "mysteriously lose the 1st item".
; This is an abbreviation for make-deocde-elements-procedure.
; TODO: Change decode-elements instead to wrap the txexpr procedures
; in a procedure that makes sure to avoid the root tag. This shouldn't
; exist at all. It's depends too closely on an implementation detail
; of pollen.
(define (make-d-el-proc proc)
  (lambda (tx) 
    (if (string-prefix? (symbol->string (get-tag tx)) "temp-tag") 
      tx
      (proc tx))))
; Turn an eql tag containing i tags into an environment where all the
; i tags become single lines of consecutive strings, and the lines are
; broken up by "\\\\\n"
; For some reason, this lops off the 1st element from the eql tag. I
; can't figure out why. Doesn't seem to matter what that first item
; is.
(define (@-flatten txexpr)
  (decode txexpr #:txexpr-proc 
                 (lambda (t) (if (is-tag? t '@) (get-elements t) t))))
(define (root . elements)
  (@-flatten
    (math-process
      (txexpr 
        'root 
        null
        (apply-tag-funcs-to-elements
          (list 
            (cons 'title
                  (lambda (tx)
                    (list '@ (apply macro 'title (get-elements tx))
                          (macro 'maketitle))))
            (cons 'eql
                  (lambda (tx)
                    (apply environment
                           'align*
                           (decode-elements 
                             (add-between
                               (get-elements tx)
                               "\\\\\n")
                             #:txexpr-proc 
                             (make-d-el-proc get-elements)))))
            (cons 'list
                  (lambda (tx)
                    (apply environment
                           (if (string=? "unordered" (attr-ref tx 'type))
                             'itemize
                             'enumerate
                             )
                           (decode-elements 
                             (get-elements tx)
                             #:txexpr-proc
                             (lambda (tx) 
                               (if (is-tag? tx 'i) 
                                 (cons "\\item" (get-elements tx))
                                 tx))))))
            (cons 'table
                  (lambda (tx)
                    (define (get-num-cols table-tag)
                      (apply max (for/list [(row (get-elements table-tag))
                                            #:when (txexpr? row)]
                                   (length (get-elements row)))))
                    (apply environment 'tabu
                           #:before-args `("to " ,(macro 'linewidth) " ")
                           #:args (list 
                                    (string-join 
                                      (add-between 
                                        (for/list [(cols (get-num-cols tx))] "X[c]")
                                        "|")))
                           (decode 
                             tx
                             #:txexpr-proc
                             (lambda (tx)
                               (define row-end "\\\\")
                               (cond [(is-tag? tx 'table)
                                      (report (list-splice
                                        (add-between 
                                          (filter 
                                            (λ (tx) (is-tag? tx '@))
                                            (get-elements tx))
                                          (list-splice row-end (macro 'hline) "\n"))))]
                                     [(is-tag? tx 'row)
                                      (list-splice
                                        (add-between 
                                          (filter 
                                            (λ (tx) (is-tag? tx '@))
                                            (get-elements tx))
                                          " & "))]
                                     [(is-tag? tx 'i) 
                                      (list-splice (get-elements tx))]
                                     [else tx])))))))
          elements)))))

(provide (all-defined-out))
