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

(define (str-check ty ex)
    (prove (: (parse-expr ex) (parse-type ty))))

(define test-slide
  (draw-proof-typey
   (inference (coloured (sequent (list (: (ref '_) (ty 'TEST))) (: (ref '_) (ty '|testity test test|))) 'blue)
              (rule '? #f)
              (list (inference (coloured (sequent '() (: (ref '_) (ty 'MOOP))) 'green)
                               (rule '? #f)
                               (list (coloured (sequent (list (: (ref '_) (ty '|MEEEP|))) (: (ref '_) (ty '|squee|))) 'black)))))))

(define a-proof
  (str-check "((A -> C) * (B -> C)) -> (A + B) -> C"
             "λp.λs.case s of left x => (fst p) x | right y => (snd p) y"))

(define hypo-examples
  (list
   (draw-proof-typey
    (inference (coloured (sequent (list (: (ref 'x) (ty 'A))) (: (ref 'x) (ty 'A))) 'black) (rule 'hypo #f) '()))
   
   (draw-proof-typey
    (inference
     (coloured (sequent (list (: (ref '_) (ty 'A))) (: (lam 'x (ref 'x)) (fun (ty 'B) (ty 'B)))) 'black)
     (intro 'fun #f)
     (list (inference (coloured (sequent (list (: (ref 'x) (ty 'B)) (: (ref '_) (ty 'A))) (: (ref 'x) (ty 'B))) 'black) (rule 'hypo #f) '()))))
   
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
         (coloured (sequent (list (: (ref 'x) (ty 'A))) (: (lam '_ (ref 'x)) (fun (ty 'B) (ty 'A)))) 'black)
         (intro 'fun #f)
         (list
          (inference
           (coloured (sequent (list (: (ref '_) (ty 'B)) (: (ref 'x) (ty 'A))) (: (ref 'x) (ty 'A))) 'black)
           (rule 'weak #f)
           (list (inference (coloured (sequent (list (: (ref 'x) (ty 'A))) (: (ref 'x) (ty 'A))) 'black) (rule 'hypo #f) '())))))
        
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
  `((test . ,(list test-slide))
    (a-proof . ,(list (draw-proof-logicy a-proof)))
    (sequents . ,(list sequents))
    (rules . ,(list rules))
    (axioms . ,(list (draw-rules axioms)))
    (hypo-examples . ,hypo-examples)
    (structural-rules . ,(list (draw-rules struct-rules)))
    (weakening-examples . ,weakening-examples)
    (function-rules . ,(list (draw-rules fun-rules)))
    (product-rules . ,(list (draw-rules prod-rules)))
    (sum-rules . ,(list (draw-rules sum-rules)))
    (cheat . ,(list cheat))))

(define slides (make-immutable-hash slide-list))

