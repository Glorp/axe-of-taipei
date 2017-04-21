#lang racket
(provide struct-rules
         fun-rules
         prod-rules
         sum-rules)

(require "infer-structs.rkt"
         "draw.rkt"
         2htdp/image)

(define (infer c r . ps)
  (inference (draw-text c)
             (draw-rule r)
             (map draw-text ps)))

(define hypothesis (infer "Γ, A ⊢ A" (rule 'hypo #f)))
(define weakening (infer "Γ, B ⊢ A" (rule 'weak #f) "Γ ⊢ A"))

(define function-intro (infer "Γ ⊢ A → B" (intro 'fun #f) "Γ, A ⊢ B"))
(define function-elim (infer "Γ ⊢ B" (elim 'fun #f) "Γ ⊢ A → B" "Γ ⊢ A"))

(define product-intro (infer "Γ ⊢ A × B" (intro 'prod #f) "Γ ⊢ A" "Γ ⊢ B"))
(define product-elim1 (infer "Γ ⊢ A" (elim 'prod "1") "Γ ⊢ A × B"))
(define product-elim2 (infer "Γ ⊢ B" (elim 'prod "2") "Γ ⊢ A × B"))

(define sum-intro1 (infer "Γ ⊢ A + B" (intro 'sum "1") "Γ ⊢ A"))
(define sum-intro2 (infer "Γ ⊢ A + B" (intro 'sum "2") "Γ ⊢ B"))
(define sum-elim (infer "Γ ⊢ C" (elim 'sum #f) "Γ ⊢ A + B" "Γ, A ⊢ C" "Γ, B ⊢ C"))


(define-syntax-rule (draw-rules (r ...) ...)
  (above (rectangle 0 32 'solid (color 0 0 0 0))
         (beside (rectangle 32 0 'solid (color 0 0 0 0))
                 (beside (above/align 'left (text (~a 'r) 14 'black) (rectangle 0 4 'solid (color 0 0 0 0)) (draw-proof r) (rectangle 0 32 'solid (color 0 0 0 0)))
                         (rectangle 32 0 'solid (color 0 0 0 0)))
                 ...)
         ...))


(define struct-rules
  (draw-rules (hypothesis weakening)))

(define fun-rules
  (draw-rules (function-intro)
              (function-elim)))

(define prod-rules
  (draw-rules (product-intro)
              (product-elim1 product-elim2)))

(define sum-rules
  (draw-rules (sum-intro1 sum-intro2)
              (sum-elim)))




   