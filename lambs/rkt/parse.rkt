#lang racket
(provide (rename-out [parse-type-string parse-type]
                     [parse-expr-string parse-expr]))
         

(require "parse/parse-expr.rkt"
         "parse/parse-type.rkt"
         "parse/str.rkt")

(define parse-type-string (compose parse-type string->str))
(define parse-expr-string (compose parse-expr string->str))

(module+ test
  (require rackunit
           "structs.rkt")
  
  (check-equal? (parse-type-string "asd × qwe × ert → hest")
                (fun (prod (prod (ty 'asd) (ty 'qwe)) (ty 'ert)) (ty 'hest)))
  
  (check-equal? (parse-expr-string "asd (qwe, λx.x x) erw")
                (app (app (ref 'asd) (pair (ref 'qwe) (lam 'x (app (ref 'x) (ref 'x))))) (ref 'erw)))

  (check-equal? (parse-type-string "asd × qwe × (ert → hest)")
                (prod (prod (ty 'asd) (ty 'qwe)) (fun (ty 'ert) (ty 'hest)))))

