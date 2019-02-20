#lang racket
(provide struct-rules
         fun-rules
         prod-rules
         sum-rules
         draw-rules
         axioms
         function-intro)

(require "infer-structs.rkt"
         "structs.rkt"
         "draw-proof.rkt"
         2htdp/image)

(define (gamma x)
  (define res (list (: (ref '_) (ty 'Γ))))
  (if x
      (cons (: (ref '_) (ty x)) res)
      res))

(define (seq x type)
  (coloured (sequent (gamma x) (: (ref '_) type))
            'black))

(define hypothesis
  (inference (seq 'A (ty 'A)) (rule 'hypo #f) '()))

(define weakening
  (inference (seq 'B (ty 'A))
             (rule 'weak #f)
             (list (seq #f (ty 'A)))))



(define function-intro
  (inference (seq #f (fun (ty 'A) (ty 'B)))
             (intro 'fun #f)
             (list (seq 'A (ty 'B)))))

(define function-elim
  (inference (seq #f (ty 'B))
             (elim 'fun #f)
             (list (seq #f (fun (ty 'A) (ty 'B)))
                   (seq #f (ty 'A)))))



(define product-intro
  (inference (seq #f (prod (ty 'A) (ty 'B)))
             (intro 'prod #f)
             (list (seq #f (ty 'A))
                   (seq #f (ty 'B)))))

(define product-elim1
  (inference (seq #f (ty 'A))
             (elim 'prod "1")
             (list  (seq #f (prod (ty 'A) (ty 'B))))))

(define product-elim2
  (inference (seq #f (ty 'B))
             (elim 'prod "2")
             (list (seq #f (prod (ty 'A) (ty 'B))))))

(define sum-intro1
  (inference (seq #f (sum (ty 'A) (ty 'B)))
             (intro 'sum "1")
             (list (seq #f (ty 'A)))))

(define sum-intro2
  (inference (seq #f (sum (ty 'A) (ty 'B)))
             (intro 'sum "2")
             (list (seq #f (ty 'B)))))

(define sum-elim
  (inference (seq #f (ty 'C))
             (elim 'sum #f)
             (list (seq #f (sum (ty 'A) (ty 'B)))
                   (seq 'A (ty 'C))
                   (seq 'B (ty 'C)))))

(define (draw-rules rows [draw draw-proof-typey])
  (define vspace (rectangle 0 12 'solid (color 0 0 0 0)))
  (define hspace (rectangle 16 0 'solid (color 0 0 0 0)))
  (define nothing (rectangle 0 0 'solid (color 0 0 0 0)))
  
  (define (draw-one r)
    (beside hspace (draw r) hspace))
  
  (define (draw-h l)
    (above vspace (apply beside nothing (map draw-one l)) vspace))
  
  (apply above nothing (map draw-h rows)))

(define axioms
  (list (list hypothesis)))

(define struct-rules
  (list (list weakening)))

(define fun-rules
  (list (list function-intro)
        (list function-elim)))


(define prod-rules
  (list (list product-intro)
        (list product-elim1 product-elim2)))

(define sum-rules
  (list (list sum-intro1 sum-intro2)
        (list sum-elim)))


