#lang at-exp racket
(provide slides
         slide-list)

(require "rules.rkt"
         "draw-proof.rkt"
         "draw.rkt"
         "infer-structs.rkt"
         "structs.rkt"
         "parse.rkt"
         "prove.rkt")

(define (parse-prove ty ex)
    (prove (: (parse-expr ex) (parse-type ty))))

(define test-slide
  (draw-proof-typey
   (inference (coloured (sequent (list (: (ref '_) (ty 'TEST))) (: (ref '_) (ty '|testity test test|))) 'blue)
              (rule '? #f)
              (list (inference (coloured (sequent '() (: (ref '_) (ty 'MOOP))) 'green)
                               (rule '? #f)
                               (list (coloured (sequent (list (: (ref '_) (ty '|MEEEP|))) (: (ref '_) (ty '|squee|))) 'black)))))))

(define a-proof
  (parse-prove "((A * B) -> C) -> A -> B -> C"
               "λf.λx.λy.f (x, y)"))

(define hypo-examples
  (list
   (draw-proof-typey
    (inference (coloured (sequent (list (: (ref 'x) (ty 'A))) (: (ref 'x) (ty 'A))) 'black) (rule 'hypo #f) '()))
   
   (draw-proof-typey
    (inference (coloured (sequent (list (: (ref 'x) (ty 'B)) (: (ref '_) (ty 'A))) (: (ref 'x) (ty 'B))) 'black) (rule 'hypo #f) '()))
   
   (draw-proof-typey
    (inference
     (coloured
      (sequent
       (list (: (ref 'x) (ty 'F)) (: (ref '_) (ty 'E)) (: (ref '_) (ty 'D)) (: (ref '_) (ty 'C)) (: (ref '_) (ty 'B)) (: (ref '_) (ty 'A)))
       (: (ref 'x) (ty 'F)))
      'black)
     (rule 'hypo #f)
     '()))))


(define weakening-examples
  (map draw-proof-typey
       (list
        
        (inference
         (coloured (sequent (list (: (ref '_) (ty 'B)) (: (ref 'x) (ty 'A))) (: (ref 'x) (ty 'A))) 'black)
         (rule 'weak #f)
         (list (inference (coloured (sequent (list (: (ref 'x) (ty 'A))) (: (ref 'x) (ty 'A))) 'black) (rule 'hypo #f) '())))
        
        (inference
         (coloured
          (sequent
           (list (: (ref '_) (ty 'F)) (: (ref '_) (ty 'E)) (: (ref '_) (ty 'D)) (: (ref '_) (ty 'C)) (: (ref '_) (ty 'B)) (: (ref 'x) (ty 'A)))
           (: (ref 'x) (ty 'A)))
          'black)
         (rule 'weak #f)
         (list
          (inference
           (coloured
            (sequent (list (: (ref '_) (ty 'E)) (: (ref '_) (ty 'D)) (: (ref '_) (ty 'C)) (: (ref '_) (ty 'B)) (: (ref 'x) (ty 'A))) (: (ref 'x) (ty 'A)))
            'black)
           (rule 'weak #f)
           (list
            (inference
             (coloured (sequent (list (: (ref '_) (ty 'D)) (: (ref '_) (ty 'C)) (: (ref '_) (ty 'B)) (: (ref 'x) (ty 'A))) (: (ref 'x) (ty 'A))) 'black)
             (rule 'weak #f)
             (list
              (inference
               (coloured (sequent (list (: (ref '_) (ty 'C)) (: (ref '_) (ty 'B)) (: (ref 'x) (ty 'A))) (: (ref 'x) (ty 'A))) 'black)
               (rule 'weak #f)
               (list
                (inference
                 (coloured (sequent (list (: (ref '_) (ty 'B)) (: (ref 'x) (ty 'A))) (: (ref 'x) (ty 'A))) 'black)
                 (rule 'weak #f)
                 (list (inference (coloured (sequent (list (: (ref 'x) (ty 'A))) (: (ref 'x) (ty 'A))) 'black) (rule 'hypo #f) '())))))))))))
        
        (inference
         (coloured
          (sequent
           (list (: (ref '_) (ty 'F)) (: (ref '_) (ty 'E)) (: (ref '_) (ty 'D)) (: (ref 'x) (ty 'C)) (: (ref '_) (ty 'B)) (: (ref '_) (ty 'A)))
           (: (ref 'x) (ty 'C)))
          'black)
         (rule 'weak #f)
         (list
          (inference
           (coloured
            (sequent (list (: (ref '_) (ty 'E)) (: (ref '_) (ty 'D)) (: (ref 'x) (ty 'C)) (: (ref '_) (ty 'B)) (: (ref '_) (ty 'A))) (: (ref 'x) (ty 'C)))
            'black)
           (rule 'weak #f)
           (list
            (inference
             (coloured (sequent (list (: (ref '_) (ty 'D)) (: (ref 'x) (ty 'C)) (: (ref '_) (ty 'B)) (: (ref '_) (ty 'A))) (: (ref 'x) (ty 'C))) 'black)
             (rule 'weak #f)
             (list
              (inference
               (coloured (sequent (list (: (ref 'x) (ty 'C)) (: (ref '_) (ty 'B)) (: (ref '_) (ty 'A))) (: (ref 'x) (ty 'C))) 'black)
               (rule 'hypo #f)
               '()))))))))))

(define function-example
  (draw-proof-typey
   (inference
    (coloured (sequent '() (: (lam 'x (ref 'x)) (fun (ty 'A) (ty 'A)))) 'black)
    (intro 'fun #f)
    (list (inference (coloured (sequent (list (: (ref 'x) (ty 'A))) (: (ref 'x) (ty 'A))) 'black) (rule 'hypo #f) '())))))

(define rules
  (draw (infer-map draw-text
                   draw-text
                   (inference "this stuff is also true"
                              "then by this rule"
                              (list "if this stuff is true")))))

(define sequents
  @~a{
 P ⊢ Q
 "from P I know Q"
 
 can be multiple things before the turnstile (⊢)
 O, P ⊢ Q
 "from O *and* P I know Q"
 
 can't be multuple things after the turnstile
 (but if there could be, it'd be like
 O, P ⊢ Q, R
 "from O *and* P I know Q *or* R")
 })

(define symbols
  @~a{
 ∧ is for conjunction, "and"
 ∨ is for disjunction, "or"
 ⊃ is for implication
 })

(define proptypes
  @~a{
 Propositions as Types
 Philip Wadler
 http://homepages.inf.ed.ac.uk/wadler/papers/propositions-as-types/propositions-as-types.pdf
      
 We can describe Howard’s observation as follows:

 • Conjunction A & B corresponds to Cartesian product A × B,
   that is, a record with two fields, also known as a pair. A proof
   of the proposition A & B consists of a proof of A and a proof of
   B. Similarly, a value of type A × B consists of a value of type
   A and a value of type B.
   
 • Disjunction A ∨ B corresponds to a disjoint sum A + B, that
   is, a variant with two alternatives. A proof of the proposition
   A ∨ B consists of either a proof of A or a proof of B, including
   an indication of which of the two has been proved. Similarly, a
   value of type A + B consists of either a value of type A or a
   value of type B, including an indication of whether this is a left
   or right summand.
   
 • Implication A ⊃ B corresponds to function space A → B. A
   proof of the proposition A ⊃ B consists of a procedure that
   given a proof of A yields a proof of B. Similarly, a value of
   type A → B consists of a function that when applied to a value
   of type A returns a value of type B.
})

(define translation
  @~a{
 so,
 instead of ⊃ we will write →
 instead of ∧ (or &) we will write ×
 instead of ∨ we will write +
 })

(define lang
  @~a{
 and we will program in a language that works something like,
 Exp u :=
   x                     variable
   
   λx.u                  abstraction, : A → B
   u1 u2                 application
   
   (u1, u2)              pair/tuple, : A × B
   fst u                 first projection
   snd u                 second projection
   
   left u                left injection, : A + B
   right u               right injection, : A + B
   case u1 of            case analysis
       left x1 => u2
     | right x2 => u3
 })

(define belt-and-suspenders
  @~a{
 Well-typed programs cannot go wrong.
 They also cannot go wrong if they cannot go at all.
 })

(define help
  @~a{
 C-e to send some stuff to the Racket-program.
 Like put your cursor within the text you wanna send,
 and have an empty line above and below.

 E.g. put cursor within the text just below and do C-e:
 
 A -> A
 λx.x
 
 
 Non-special things will be parsed and typechecked and so on.
 Special commands begin with #
 
 #:scale <n>
 scale new images by a factor of <n>
 
 #:scale
 show current #:scale setting
 
 #:drawings <x>
 change how proof-things are drawn. <x> can be typey, termy, or logicy

 #:drawings
 show current #:drawing setting
 
 #:slide <name>
 show the slide named <name>
 
 #:slide
 show names of available slides
 
 #:it
 or <M-up>
 insert the last type/expression-thing-text you sent to Racket
 
 #:q
 stop the Racket-thing
 })

(define cheat
  @~a{
 A → A
 λx.x


 (A × B → C) → A → B → C
 λf.λa.λb.f (a, b)



 (A -> C) * (B -> C) -> A + B -> C
 λp.λs.case s of
           left x => (fst p) x
         | right x => (snd p) x
 })

(define slide-list
  `((help ,help)
    (test ,test-slide)
    (a-proof ,(draw-proof-logicy a-proof))
    (symbols ,symbols)
    (sequents ,sequents)
    (rules ,rules)
    (proptypes ,proptypes)
    (translation ,translation)
    (a-proof-typey ,(draw-proof-typey a-proof))
    (lang ,lang)
    (axioms ,(draw-rules axioms))
    (hypo-examples . ,hypo-examples)
    (structural-rules ,(draw-rules struct-rules))
    (weakening-examples . ,weakening-examples)
    (function-rules ,(draw-rules fun-rules))
    (function-example ,function-example)
    (product-rules ,(draw-rules prod-rules))
    (sum-rules ,(draw-rules sum-rules))
    (belt-and-suspenders ,belt-and-suspenders)
    (cheat ,cheat)))

(define slides (make-immutable-hash slide-list))

