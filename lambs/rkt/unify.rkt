#lang racket
(provide unify)

(require "structs.rkt"
         "opt.rkt")

(define (unify a b)
  (match* (a b)
    [((wild) x) x]
    [(x (wild)) x]
    [((fun ad ar) (fun bd br))
     (opts> ((unify ad bd) (unify ar br))
            [(new-d new-r) (fun new-d new-r)])]
    [((prod aa ab) (prod ba bb))
     (opts> ((unify aa ba) (unify ab bb))
            [(new-a new-b) (prod new-a new-b)])]
    [((sum ll lr) (sum rl rr))
     (opts> ((unify ll rl) (unify lr rr))
            [(new-l new-r) (sum new-l new-r)])]
    [(_ _) (and (equal? a b) a)]))

(module+ test
  (require rackunit)
  (check-equal? (unify (prod (wild) 'x) (prod (fun 'a 'b) 'x))
                (fun (fun 'a 'b) 'x))
  (check-equal? (unify (prod (wild) 'x) (prod (fun 'a 'b) 'y))
                #f))