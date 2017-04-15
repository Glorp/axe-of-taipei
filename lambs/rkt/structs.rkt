#lang racket
(provide expr?
         type?
         (struct-out ref)
         (struct-out lam)
         (struct-out app)
         (struct-out pair)
         (struct-out fst)
         (struct-out snd)
         (struct-out left)
         (struct-out right)
         (struct-out case)
         (struct-out fun)
         (struct-out prod)
         (struct-out sum)
         (struct-out ty)
         (struct-out wild)
         (struct-out :)
         (struct-out sequent))

(struct lam (p x) #:transparent)
(struct app (f a) #:transparent)
(struct pair (f s) #:transparent)
(struct fst (x) #:transparent)
(struct snd (x) #:transparent)
(struct ref (s) #:transparent)
(struct left (x) #:transparent)
(struct right (x) #:transparent)
(struct case (x lp l rp r) #:transparent)

(define (expr? x)
  (match x
    [(lam p x) (and (symbol? p) (expr? x))]
    [(app f a) (and (expr? f) (expr? a))]
    [(pair f s) (and (expr? f) (expr? s))]
    [(fst x) (expr? x)]
    [(snd x) (expr? x)]
    [(ref s) (symbol? s)]
    [(left x) (expr? x)]
    [(right x) (expr? x)]
    [(case x lp l rp r)
     (and (expr? x)
          (symbol? lp)
          (expr? l)
          (symbol? rp)
          (expr? r))]
    [_ #f]))

(struct fun (d r) #:transparent)
(struct prod (a b) #:transparent)
(struct sum (l r) #:transparent)
(struct ty (name) #:transparent)
(struct wild () #:transparent)

(define (type? x)
  (match x
    [(fun d r) (and (type? d) (type? r))]
    [(prod a b) (and (type? a) (type? b))]
    [(sum l r) (and (type? l) (type? r))]
    [(ty s) (symbol? s)]
    [(wild) #t]
    [_ #f]))

(struct : (term type) #:transparent)
(struct sequent (antecedents consequent) #:transparent)
