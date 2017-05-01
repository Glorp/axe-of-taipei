#lang racket
(provide prove)

(require "structs.rkt"
         "infer-structs.rkt"         
         "unify.rkt")

(define (lookup term/type context)
  (match term/type
    [(: s expected)
     (define (pred x)
       (equal? s x))
     (match context
    
       ['()
        (inference (coloured (sequent context term/type) 'red)
                   (rule '? #f)
                   '())]
    
       [(list (: (? pred) t) xs ...)
        (inf-halp t
                  (sequent context term/type)
                  (rule 'hypo #f))]
    
       [(list x xs ...)
        (define proof (lookup term/type xs))
        (inf-halp (proof-type proof)
                  (sequent context term/type)
                  (rule 'weak #f)
                  proof)])]))

(define (proof-type p)
  (match p
    [(inference (coloured (sequent _ (: _ t)) _) _ _) t]))

(define (inf-halp found seq rule . ps)
  (match seq
    [(sequent as (: exp expected))
     (define unif (unify expected found))
     (define new-type (if unif unif expected))
     (define col (if unif 'black 'red))
     (inference (coloured (sequent as (: exp new-type)) col) rule  ps)]))

(define (prove term/type [context '()])
  (match term/type
    [(: (lam p x) (fun d r))
     (define proof (prove (: x r) (cons (: (ref p) d) context)))
     (inf-halp (fun d (proof-type proof))
               (sequent context term/type)
               (intro 'fun #f)
               proof)]
    
    [(: (pair f s) (prod a b))
     (define aproof (prove (: f a) context))
     (define bproof (prove (: s b) context))
     (inf-halp (prod (proof-type aproof) (proof-type bproof))
               (sequent context term/type)
               (intro 'prod #f)
               aproof
               bproof)]

    [(: (left l) (sum a b))
     (define proof (prove (: l a) context))
     (inf-halp (sum (proof-type proof) b)
               (sequent context term/type)
               (intro 'sum "1")
               proof)]
    
    [(: (right r) (sum a b))
     (define proof (prove (: r b) context))
     (inf-halp (sum a (proof-type proof))
               (sequent context term/type)
               (intro 'sum "2")
               proof)]
    
    [(: (ref s) _) (lookup term/type context)]
    
    [(: (fst x) expected)
     (define proof (prove (: x (prod expected (wild))) context))
     (inf-halp (prod-a (proof-type proof))
               (sequent context term/type)
               (elim 'prod "1")
               proof)]
    
    [(: (snd x) expected)
     (define proof (prove (: x (prod (wild) expected)) context))
     (inf-halp (prod-b (proof-type proof))
               (sequent context term/type)
               (elim 'prod "2")
               proof)]

    [(: (case x lp l rp r) expected)
     (define proof (prove (: x (sum (wild) (wild))) context))
     (define t (proof-type proof))
     (define lt (sum-l t))
     (define rt (sum-r t))
     
     (define lproof (prove (: l expected) (cons (: (ref lp) lt) context)))
     (define rproof (prove (: r expected) (cons (: (ref rp) rt) context)))

     (define unif1 (unify expected (proof-type lproof)))
     (define unif2 (and unif1 (unify unif1 (proof-type rproof))))
     
     (inference (coloured (sequent context term/type) (if unif2 'black 'red))
                (elim 'sum #f)
                (list proof lproof rproof))]
    
    [(: (app f a) expected)
     (define fproof (prove (: f (fun (wild) expected)) context))
     (define aproof (prove (: a (fun-d (proof-type fproof))) context))
     
     (inf-halp (fun-r (proof-type fproof))
               (sequent context term/type)
               (elim 'fun #f)
               fproof
               aproof)]
    
    [_
     (inference (coloured (sequent context term/type) 'red)
                (rule '? #f)
                '())]))

(module+ main
  (require "draw-proof.rkt"
           "parse.rkt")

  (define (check-txt ty ex)
    (prove (: (parse-expr ex) (parse-type ty))))
  
  (define (check ty ex)
    (draw-proof-typey (prove (: (parse-expr ex) (parse-type ty)))))

  (check "A -> A + B"
         "λa.left a")
 
  (check "A → B → A"
         "λx.λy.x")
  
  (check "A → B → A"
         "λx.λy.y")

  (check "(A × B) → B"
         "λp.snd p")
  
  (check "(A → B) → A → B"
         "λf.λx.f x")

  (check "(A × B → C) → A → B → C"
         "λf.λa.λb.f (a, b)")

  (check "(A -> C) * (B -> C) -> A + B -> C"
         "λp.λs.case s of left x => (fst p) x | right x => (snd p) x")

  (check "(A -> C) * (B -> C) -> A + B -> C"
         "λp.λs.case s of left x => (fst p) x | right x => (fst p) x")

  (check "((A -> A) -> A -> A) -> ((A -> A) -> A -> A)"
         "λn.λf.λx.f (n f x)")
  (check-txt "A -> A"
             "λx.x"))

  

