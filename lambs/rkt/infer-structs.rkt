#lang racket
(provide (struct-out rul)
         rule
         (struct-out inf)
         infer
         infer-map)

(struct rul (r i) #:transparent)

(define (rule r (i #f))
  (rul r i))

(struct inf (c r ps color) #:transparent)
(define (infer c r #:color [color 'black] . ps)
  (inf c r ps color))

(define (infer-map f i)
  (match i
    [(inf c r ps color)
     (inf (f c) r (map (λ (x) (infer-map f x)) ps) color)]))