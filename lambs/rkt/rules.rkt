#lang racket
(require "infer-structs.rkt"
         "draw.rkt"
         2htdp/image)

(define hypothesis (infer "Γ, A ⊢ A" (rule "H")))
(define weakening (infer "Γ, B ⊢ A" (rule "W") "Γ ⊢ A"))

(define function-intro (infer "Γ ⊢ A → B" (rule "→I") "Γ, A ⊢ B"))
(define function-elim (infer "Γ ⊢ B" (rule "→E") "Γ ⊢ A → B" "Γ ⊢ A"))

(define product-intro (infer "Γ ⊢ A × B" (rule "×I") "Γ ⊢ A" "Γ ⊢ B"))
(define product-elim1 (infer "Γ ⊢ A" (rule "×E" "1") "Γ ⊢ A × B"))
(define product-elim2 (infer "Γ ⊢ B" (rule "×E" "2") "Γ ⊢ A × B"))

(define sum-intro1 (infer "Γ ⊢ A + B" (rule "+I" "1") "Γ ⊢ A"))
(define sum-intro2 (infer "Γ ⊢ A + B" (rule "+I" "2") "Γ ⊢ B"))
(define sum-elim (infer "Γ ⊢ C" (rule "+E") "Γ ⊢ A + B" "Γ, A ⊢ C" "Γ, B ⊢ C"))


(define-syntax-rule (draw-rules (r ...) ...)
  (above (rectangle 0 32 'solid (color 0 0 0 0))
         (beside (rectangle 32 0 'solid (color 0 0 0 0))
                 (beside (above/align 'left (text (~a 'r) 14 'black) (rectangle 0 4 'solid (color 0 0 0 0)) (draw r) (rectangle 0 32 'solid (color 0 0 0 0)))
                         (rectangle 32 0 'solid (color 0 0 0 0)))
                 ...)
         ...))

(define rules-img
  (draw-rules (hypothesis weakening)
              (function-intro function-elim)
              (product-intro product-elim1 product-elim2)
              (sum-intro1 sum-intro2 sum-elim)))



   