#lang racket
(provide slides)

(require "rules.rkt"
         "draw-proof.rkt"
         "draw.rkt"
         "infer-structs.rkt"
         "structs.rkt")

(define test-slide
  (draw-proof-typey
   (inference (coloured (sequent (list (: (ref '_) (ty 'TEST))) (: (ref '_) (ty '|testity test test|))) 'blue)
              (rule '? #f)
              (list (inference (coloured (sequent '() (: (ref '_) (ty 'MOOP))) 'green)
                               (rule '? #f)
                               (list (coloured (sequent (list (: (ref '_) (ty '|MEEEP|))) (: (ref '_) (ty '|squee|))) 'black)))))))

(define rules
  (draw (infer-map draw-text
                   draw-text
                   (inference "this stuff is also true"
                              "then by this rule"
                              (list "if this stuff is true")))))

(define sequents
  (string-join (list "P ⊢ Q"
                     "\"from P I know Q\""
                     ""
                     "can be multiple things before the turnstile (⊢)"
                     "O, P ⊢ Q"
                     "\"from O *and* P I know Q\""
                     ""
                     "can't be multuple things after the turnstile"
                     "(but if there could be, it'd be like"
                     "O, P ⊢ Q, R"
                     "\"from O *and* P I know Q *or* R\")")
               "\n"))


(define slides
  (make-immutable-hash
   `((test . ,(list test-slide))
     (sequents . ,(list sequents))
     (rules . ,(list rules))
     (axioms . ,(list (draw-rules axioms)))
     (structural-rules . ,(list (draw-rules struct-rules)))
     (function-rules . ,(list (draw-rules fun-rules)))
     (product-rules . ,(list (draw-rules prod-rules)))
     (sum-rules . ,(list (draw-rules sum-rules))))))

