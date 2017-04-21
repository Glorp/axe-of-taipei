#lang racket
(provide slides)

(require "rules.rkt"
         "draw-proof.rkt"
         "infer-structs.rkt"
         "structs.rkt")

(define test-slide
  (draw-proof-typey
   (inference (coloured (sequent (list (: (ref '_) (ty 'TEST))) (: (ref '_) (ty '|testity test test|))) 'blue)
              (rule '? #f)
              (list (inference (coloured (sequent '() (: (ref '_) (ty 'MOOP))) 'green)
                               (rule '? #f)
                               (list (coloured (sequent (list (: (ref '_) (ty '|MEEEP|))) (: (ref '_) (ty '|squee|))) 'black)))))))

(define slides
  (make-immutable-hash
   `((structural-rules . ,(list (draw-rules struct-rules)))
     (function-rules . ,(list (draw-rules fun-rules)))
     (product-rules . ,(list (draw-rules prod-rules)))
     (sum-rules . ,(list (draw-rules sum-rules)))
     (test . ,(list test-slide)))))

