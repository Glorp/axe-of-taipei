#lang racket
(provide (struct-out rule)
         (struct-out inference)
         (struct-out coloured)
         infer-map)

(struct rule (name number) #:transparent)

(struct coloured (thing colour) #:transparent)

(struct inference (conclusion rule premises) #:transparent)

(define (infer-map cf rf i)
  (match i
    [(inference c r ps)
     (inference (cf c) (rf r) (map (λ (x) (infer-map cf rf x)) ps))]))