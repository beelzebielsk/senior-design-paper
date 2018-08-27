#lang racket
(require txexpr 
         "latex-commands.rkt"
         "utility.rkt"
         pollen/tag
         pollen/decode
         racket/stxparam)

(define-syntax ddlog
  (lambda (stx)
    (syntax-case stx ()
      [name (identifier? #'name) #'"DDLOG"]
      [(_ args ...)
       #'(ensure-math
           "DDLOG" args ...)])))
          ;(format "\\DDLOG(~a)" 
                  ;(string-join (map ~a (list args ...)) " ")))])))
(define-syntax key
  (lambda (stx)
    (syntax-case stx ()
      [name (identifier? #'name) #'(ensure-math (macro 'lambda))]
      [(_ index) 
       #'(ensure-math 
           (format "~a^{(~a)}" (macro 'lambda) index))])))
(define-syntax input
  (lambda (stx)
    (syntax-case stx ()
      [name 
        (identifier? #'name) 
        #'(ensure-math (macro 'mathbb "I"))]
      [(_ args ...) 
       #'(ensure-math (macro 'mathbb "I") "(" args ... ")")])))
(define-syntax-rule (report EXPR)
  (let [(result EXPR)] (displayln result) result))

(define-tag-function 
  (math attrs elems)
  (txexpr 'math attrs elems))
(define ensure-math math)
;(define (ensure-math . _)
  ;(txexpr 'math null _))

(define-syntax-rule (define-math-tag (name attrs elems) body ...)
  (define-tag-function 
    (name attrs elems) 
    (ensure-math ((lambda (attrs elems) body ...) attrs elems))))

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
(define (math-process xexpr)
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
                 (append result 
                         (list "$") 
                         (get-elements (helper elem #t))
                         (list "$")))
               (append result (list elem))))
             null (get-elements xexpr)))
        xexpr))
  (helper xexpr #f))

; ensure-math should expand to math, unless inside of a math tag.
; Can be accomplished through syntax parameters.
; ensure-math will be syntax parameter which expands to a math tag.
; math tags change that syntax parameter to be something
; inconsequential.

(define-tag-function 
  (math-tag attrs elements)
  (txexpr 'math attrs elements))
(define-tag-function ($ _ text)
  (apply math text))
(define-tag-function ($$ _ text)
  (apply math #:display "" text))
(define-math-tag (share _ text)
  (format "<~a>" (string-join (map ~a text) " ")))
(define-math-tag (secret-share _ text)
  (apply share (flatten (list key "(" text ")"))))
(define-math-tag (set _ text)
  (string-append
    "\\{"
    (string-join text ", ")
    "\\}"))
(define mult (ensure-math (macro 'otimes)))
(define-math-tag (encryption _ text)
  (define content (string-join (map ~a text) " "))
  (format "[~a]_{~a}" content key))
(define-math-tag 
  (congruent attrs text)
  (let [(m (if (attrs-have-key? 'modulus attrs)
             (attr-ref 'modulus)
             "n"))]
    text))


(define location (ensure-math "M"))
(define-math-tag (bit _ text)
  (define (norm-arguments args)
    (cond [(>= (length args) 2)
           (take args 2)]
          [(and (= 1 (length args))
                (string-contains? (first args) " "))
           (norm-arguments (string-split (first args)))]
          [else null]))
  (let [(args (norm-arguments text))]
    (displayln (~v text))
    (displayln (~v args))
    (if (null? args)
      (error "Tag `bit` takes two space-separated arguments"
             text)
      (match-let [((list base index) args)]
        (format "~a^{(~a)}" base index)))))
(define-math-tag 
  (sum attrs text) 
  "")







(define (final-eql eql-tag)
  (define break-equation "\\\\\n")
  (define (item-func t)
      (string-join (get-elements t) " "))
    (environment 
      'align
      (string-join 
        (map item-func (get-elements eql-tag))
        break-equation)))



; TODO:
; - Decode tables to turn single lines into cells, and allow for cell
;   tags to appear.
; - Decode lists to turn bullets into items, and still allow for item
;   tags to be typed manually.
; - Decode eql to turn lines into items, and still alow for item tags
;   to be typed manually.
; - eql -> eqarray
;   ol -> enumerate
;   l -> itemize
;   i -> $ of in eql, i -> \\item if in ol or l.
;   macro -> transform straightforwardly
;   environment -> transform straightforwardly
; - Don't use complicated tag functions unless necessary. Transform
;   the tags to a small number of core tag functions that would be
;   handled by a `->latex` function. Handle some of the context
;   sensitive stuff within the tag itself, and some of it as
;   postprocessing after all the tags have done their work, inside of
;   the 'root tag function. Right now, something like the 'name tag in
;   resume would probably require a macro, though it could be handled
;   by something like decoding. the 'eql tag is better handled at the
;   tag-level, because one of the problems I've solving is intermixing
;   lines and the 'i tag as items to the eql.
; - Use the tags to handle "preprocessing". Getting everything in such
;   a way that's "just right" for postprocessing to handle.
;   Postprocessing will be responsible for translating things into the
;   stuff that decides the appearance of shit. Preprocessing will make
;   that job easier, as easy as possible.
; - I'm def moving my way toward the idea of "steps of processing".
;   But the locations where the processing takes place doesn't have to
;   be fixed. For instance, while working on eql, I'm thinking of
;   having an initial step which creates all the proper 'i tags, and
;   then have another function which processes the 'i tags
;   appropriately. My initial thought was to let the 'root tag handle
;   that at the end, and perform context sensitive processing there.
;   However, I can just create more than one tag function, and have
;   the initial tag function do some initial decoding work, which
;   would alter the contents of the real eql tag to look like I wrote
;   everything perfectly and responsibly, and then let a final eql tag
;   handle the transformation of all the tags into what should appear
;   in the final output. More concretely: 
;       (final-eql
;         (initial-eql elements)) ; wraps stuff in 'i tags
;   I'm still thinking about doing that "core" of basic output
;   elements. If I did something like that, then I could build all the
;   tags on top of those, and straightforwardly transform those into
;   the output that ought to be. So... controlling output would be one
;   job, dictating how tags transform to that small core set of
;   tags would be another job, and transforming that small core set of
;   tags into actual output would be a 3rd job, all somewhat
;   independent from each other. Is that design worth it? It might be.
;   For one, I could evolve a "middle layer" output language and just
;   figure out how to produce output for different formats from that
;   middle layer. That would make poly output much easier to produce
;   and a lot less confusing to read.
; - Preprocessing is cleaning up the custom tags so that they're very
;   easy to postprocess, and turning everything else into tags that
;   are readily translatable into tex (macro, environment, or just
;   plain text).
; - Postprocessing is performing any contextual transformations that
;   are necessary, and then transforming the result of that into the
;   final output.

; - Core tags
;   - macro
;   - environment
;   - math
; - Convenience tags
;   - ol (ordered list)
;   - l (unordered list)
;   - eql (equation list)
;       - i (as a member of ol, l, or eql)
;   - table
;       - row, and cell, as members of table
; Everything else should trasnform pretty diectly into a rendered
; form. Putting it into a tag is optional.
    
(define (eql . text)
  (displayln (format "eql contents: ~v" text))
  (displayln (format "eql split at newlines: ~v" 
                     (split-list-at-tag-or-newline text)))
  ; contextual processing: if a printable? or series of printable?
  ; is/are followed by a newline or a txexpr?, then wrap that in an
  ; 'i tag. The following is done under the assumption that the only
  ; elements in text are txexpr? and printable?. 
  (let [(other-splits
          (split-where 
            text
            (λ (current . _)
               (or (and (string? current) (string=? "\n" current))
                   (txexpr? current)))
            #:keep-where
            (λ (current . _)
               (if (txexpr? current) 'separate #f))
            #:split-map
            (λ (current . _) 
               (if ((listof printable?) current)
                 (txexpr 'i null (map ~a current))
                 current))))]
    (txexpr 'eql null other-splits)))
; operates on a list of elements that should be contained in a list,
; and then returns a list of 'i tags.
#|
(define (decode-list-items elements)
  (split-where
    elements
    (λ (elem current-split _)
       (and (string? elem) 
            (regexp-match "^\s+-" elem)
            (string=? "\n "(last current-split))))
    #:keep-where (λ _ 'next)
    #:split-map
    (λ (current . _)
       (txexpr 'i
               null
               (cons
                 (regexp-replace "^\s+-" (first current) "")
                 (rest current))))))
|#

(define (split-list-at-tag-or-newline lst)
  (split-where 
    lst 
    (λ (current . _) 
       (or (and (string? current) (string=? "\n" current))
           (is-tag? current 'i)))
    #:keep-where 
    (λ (split-elem . _) 
       (if (txexpr? split-elem) 'separate #f))))
(define (split-list-at-bullets-or-list-tags lst bullet-pattern list-tag)
  (split-where
    lst
    (λ (elem current-split . _)
       (displayln (~v elem))
       (displayln (~v current-split))
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
  (displayln (~v elems))
  (txexpr 'list '((type "unordered")) 
          (report (split-list-at-bullets-or-list-tags elems 
                                                      #px"^\\s*-" 
                                                      'i))))
(define-tag-function
  (ol attrs elems)
  (txexpr 'list '((type "ordered")) 
          (split-list-at-bullets-or-list-tags elems #px"^\\s*-" 'i)))

(provide (all-defined-out))
