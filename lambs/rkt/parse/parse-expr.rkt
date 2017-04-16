#lang racket
(provide parse-expr)

(require "../structs.rkt"
         "../opt.rkt"
         "str.rkt"
         "util.rkt")

(struct tup () #:transparent)
(struct cas () #:transparent)
(struct of () #:transparent)
(struct lef () #:transparent)
(struct rig () #:transparent)
(struct bar () #:transparent)
(struct fs () #:transparent)
(struct sn () #:transparent)
(struct casething () #:transparent)

(define (appy f a)
  (match f
    [(fs) (fst a)]
    [(sn) (snd a)]
    [(lef) (left a)]
    [(rig) (right a)]
    [_ (app f a)]))

(define (case-expr l)
  (match l
    [(list (cas) xs ...)
     (opt>
      (split-list of? xs)
      [of-split
       (match of-split
         [(list before after)
          (opt>
           (split-list bar? after)
           [barsplit
            (match barsplit
              [(list (list (lef) (ref lp) (casething) ls ...) (list (rig) (ref rp) (casething) rs ...))
               (opts>
                ((expr-list->expr before) (expr-list->expr ls) (expr-list->expr rs))
                [(x l r) (case x lp l rp r)])]
              [_ #f])])]
                
         [_ #f])])]
    [(list xs ...) #f]))

(define (expr-list->expr expr-l)
  (opt>
   (split-list tup? expr-l)
   [split
    (left-assoc pair
                (map (λ (l) (left-assoc appy l))
                     split))]))
  
(define (parse-expr x)
  (define res
    (opt>
     (read-expr-list x)
     [expr-l
      (match (case-expr expr-l)
        [#f (expr-list->expr expr-l)]
        [x x])]))
  (and (expr? res) res))

(define (read-expr-list y)
  (define ((add-expr x) rest-str)
    (opt> (read-expr-list rest-str)
          [rest (cons x rest)]))
  
  (define (halp y)
    (define x (skip-whites y))
    (cond [(str-empty? x) '()]
          
          [(equal? (str-current x) #\()
           (opt> ((split #\)) (next x))
                 [(cons inside rest)
                  (opts>
                   ((parse-expr inside) (halp rest))
                   [(a d) (cons a d)])])]
          
          [(equal? (str-current x) #\λ)
           (opt>
            (read-lambda x)
            [l (list l)])]
             
          [((expect-string ",") x) => (add-expr (tup))]
          [((expect-string "fst") x) => (add-expr (fs))]
          [((expect-string "snd") x) => (add-expr (sn))]
          
          
          [((expect-string "case") x) => (add-expr (cas))]
          [((expect-string "of") x) => (add-expr (of))]
          [((expect-string "left") x) => (add-expr (lef))]
          [((expect-string "right") x) => (add-expr (rig))]
          [((expect-string "↪") x) => (add-expr (casething))]
          [((expect-string "=>") x) => (add-expr (casething))]
          [((expect-string "|") x) => (add-expr (bar))]
             
          [else
           (match (read-word x)
             [#f #f]
             [(cons w rest)
              (opt>
               (halp rest)
               [rest-list (cons (ref (string->symbol w)) rest-list)])])]))
  (match (halp y)
    ['() #f]
    [x x]))

(define (read-lambda x)
  (opt>
   ((expect-string "λ") x)
   [lam-rest
    (opt> (read-word lam-rest)
    [(cons param param-rest)
     (opt>
      ((expect-string ".") param-rest)
      [body
       (opt>
        (parse-expr body)
        [l (lam (string->symbol param) l)])])])]))
