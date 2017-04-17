#lang racket
(provide (struct-out rule)
         (struct-out inference)
         (struct-out coloured)
         (struct-out rule)
         (struct-out intro)
         (struct-out elim)
         infer-map)

(struct rule (name number) #:transparent)
(struct intro (name number) #:transparent)
(struct elim (name number) #:transparent)

(struct coloured (thing colour) #:transparent)

(struct inference (conclusion rule premises) #:transparent)

(define (infer-map cf rf i)
  (match i
    [(inference c r ps)
     (inference (cf c) (rf r) (map (λ (x) (infer-map cf rf x)) ps))]))