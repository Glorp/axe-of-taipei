#lang racket
(provide prove)

(require "structs.rkt"
         "parse.rkt"
         "unparse.rkt"
         "infer-structs.rkt"
         "draw.rkt"
         "opt.rkt"
         "unify.rkt")

(define (lookup term/type context)
  (match term/type
    [(: s expected)
     (define (pred x)
       (equal? s x))
     (match context
    
       ['()
        (inference (coloured (sequent context term/type) 'red)
                   (rule "?" #f)
                   '())]
    
       [(list (: (? pred) t) xs ...)
        (inf-halp t
                  (sequent context term/type)
                  (rule "H" #f))]
    
       [(list x xs ...)
        (define proof (lookup term/type xs))
        (inf-halp (proof-type proof)
                  (sequent context term/type)
                  (rule "W" #f)
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
    [(: exp expected)
     (match* (expected exp)
       [((fun d r) (lam p x))
        (define proof (prove (: x r) (cons (: (ref p) d) context)))
        (inf-halp (fun d (proof-type proof))
                  (sequent context term/type)
                  (rule "→I" #f)
                  proof)]
    
       [((prod a b) (pair f s))
        (define aproof (prove (: f a) context))
        (define bproof (prove (: s b) context))
        (inf-halp (prod (proof-type aproof) (proof-type bproof))
                  (sequent context term/type)
                  (rule "×I" #f)
                  aproof
                  bproof)]

       [((sum a b) (left l))
        (define proof (prove (: l a) context))
        (inf-halp (sum (proof-type proof) b)
                  (sequent context term/type)
                  (rule "+I" "1")
                  proof)]

       [((sum a b) (right r))
        (define proof (prove (: r b) context))
        (inf-halp (sum a (proof-type proof))
                  (sequent context term/type)
                  (rule "+I" "2")
                  proof)]
    
       [(_ (ref s)) (lookup term/type context)]
    
       [(_ (fst x))
        (define proof (prove (: x (prod expected (wild))) context))
        (inf-halp (prod-a (proof-type proof))
                  (sequent context term/type)
                  (rule "×E" "1")
                  proof)]
    
       [(_ (snd x))
        (define proof (prove (: x (prod (wild) expected)) context))
        (inf-halp (prod-b (proof-type proof))
                  (sequent context term/type)
                  (rule "×E" "1")
                  proof)]

       [(_ (case x lp l rp r))
        (define proof (prove (: x (sum (wild) (wild))) context))
        (define t (proof-type proof))
        (define lt (sum-l t))
        (define rt (sum-r t))
        (inference (coloured (sequent context term/type) 'black)
                   (rule "+E" #f)
                   (list proof
                         (prove (: l expected) (cons (: (ref lp) lt) context))
                         (prove (: r expected) (cons (: (ref rp) rt) context))))]

       [(_ (app f a))
        (define fproof (prove (: f (fun (wild) expected)) context))
        (define aproof (prove (: a (fun-d (proof-type fproof))) context))
        (inference (coloured (sequent context term/type) 'black)
                   (rule "→E" #f)
                   (list fproof aproof))]
               
    
       [(_ _)
        (and
         (inference (coloured (sequent context term/type) 'red)
                    (rule "?" #f)
                    '()))])]))

(module+ main
  (define (prooove ty ex)
    (prove (: (parse-expr ex) (parse-type ty))))
  
  (define (check ty ex)
    (define str-proof (infer-map (draw-coloured-sequent sequent->termstring)
                                 (λ (x) (draw-rule x 'black))
                                 (prooove ty ex)))
    (draw str-proof))

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
         "λp.λs.case s of left x => (fst p) x | right x => (fst p) x"))

  

