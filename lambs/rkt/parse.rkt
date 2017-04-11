#lang racket
(provide (rename-out [read-type-string parse-type]
                     [read-expr-string parse-expr]))
         

(require "parse/parse-expr.rkt"
         "parse/parse-type.rkt"
         "parse/str.rkt"
         "util.rkt")

(define read-type-string (>> string->str read-type))
(define read-expr-string (>> string->str read-expr))

(module+ test
  (require rackunit
           "structs.rkt")
  
  (check-equal? (read-type-string "asd × qwe × ert → hest")
                (fun (prod (prod 'asd 'qwe) 'ert) 'hest))
  
  (check-equal? (read-expr-string "asd (qwe, λx.x x) erw")
                (app (app (ref 'asd) (pair (ref 'qwe) (lam 'x (app (ref 'x) (ref 'x))))) (ref 'erw)))

  (check-equal? (read-type-string "asd × qwe × (ert → hest)")
                (prod (prod 'asd 'qwe) (fun 'ert 'hest))))

