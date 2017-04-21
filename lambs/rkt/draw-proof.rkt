#lang racket
(provide draw-proof-typey
         draw-proof-termy
         draw-proof-logicy)

(require "draw.rkt"
         "unparse.rkt"
         "structs.rkt"
         "infer-structs.rkt")

(define (draw-proof-typey p)
  (draw (infer-map (draw-coloured-sequent sequent->typestring)
                   draw-type-rule
                   p)))

(define (draw-proof-termy p)
  (draw (infer-map (draw-coloured-sequent sequent->termstring)
                   draw-type-rule
                   p)))

(define (draw-proof-logicy p)
  (draw (infer-map (draw-coloured-sequent sequent->logicstring)
                   draw-logic-rule
                   p)))

