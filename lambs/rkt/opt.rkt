#lang racket
(provide opt>
         opts>
         opt-map)

(define-syntax-rule (opt> arg clause ...)
  (match arg
    [#f #f]
    clause
    ...))

(define-syntax opts>
  (syntax-rules ()
    [(_ () (clause ...) #:arglist (arglist ...))
     (match* (arglist ...)
       clause
       ...)]
    [(_ (arg args ...) (clause ...) #:arglist (arglist ...))
     (let ([x arg])
       (match x
         [#f #f]
         [_ (opts> (args ...) (clause ...) #:arglist (arglist ... x))]))]
    [(_ args clause ...)
     (opts> args (clause ...) #:arglist ())]))

(define (opt-map f l)
  (match l
    ['() '()]
    [(list x xs ...)
     (opts>
      ((f x) (opt-map f xs))
      [(a d) (cons a d)])]))
