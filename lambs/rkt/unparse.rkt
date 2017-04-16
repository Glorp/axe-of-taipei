#lang racket
(require "structs.rkt")
(provide unparse-expr
         unparse-type
         sequent->string
         sequent->typestring
         sequent->termstring)

(define (paren s)
  (~a "(" s ")"))

(define (unparse-expr x)
  (match x
    [(lam p x) (format "λ~a.~a" p (unparse-expr x))]
    [(app f a) (~a (unparse-expr f) " " (argstring a))]
    [(pair f s) (~a (unparse-expr f) ", " (unparse-expr s))]
    [(fst x) (~a "fst " (argstring x))]
    [(snd x) (~a "snd " (argstring x))]
    [(left x) (format "left ~a" (argstring x))]
    [(right x) (format "right ~a" (argstring x))]
    [(case x lp l rp r) (format "case ~a of left ~a ↪ ~a | right ~a ↪ ~a"
                                (unparse-expr x)
                                lp
                                (unparse-expr l)
                                rp
                                (unparse-expr r))]
    [(ref s) (~a s)]))

(define (argstring x)
  (match x
    [(ref s) (~a s)]
    [x (paren (unparse-expr x))]))

(define (sndstring x)
  (match x
    [(lam _ _) (unparse-expr x)]
    [x (argstring x)]))

(define (unparse-type x)
  (match x
    [(fun (fun dd dr) r) (format "~a → ~a" (paren (unparse-type (fun dd dr))) (unparse-type r))]
    [(fun d r) (format "~a → ~a" (unparse-type d) (unparse-type r))]
    [(prod a b) (format "~a × ~a" (type-argstring a) (type-argstring b))]
    [(sum l r) (format "~a + ~a" (type-argstring l) (type-argstring r))]
    [(wild) "?"]
    [(ty s) (~a s)]))

(define (type-argstring x)
  (match x
    [(wild) (unparse-type x)]
    [(ty _) (unparse-type x)]
    [_ (paren (unparse-type x))]))

(define ((sequent->string term/type->string) s)
  (define (context->string c)
    (string-join (map term/type->string (reverse c)) ", "))
  (match s
    [(sequent c x)
     (format "~a ⊢ ~a" (context->string c) (term/type->string x))]))

(define sequent->typestring
  (sequent->string
   (λ (x)
     (match x
      [(: _ t) (unparse-type t)]))))

(define sequent->termstring
  (sequent->string
   (λ (x)
     (match x
      [(: x t) (format "~a : ~a" (unparse-expr x) (unparse-type t))]))))

(module+ main
  (require "parse.rkt")
  (define ((back-forth back forth) x)
    (printf "   ~a~n=> ~a~n=> ~a~n=> ~a~n~n"
            x
            (forth x)
            (back (forth x))
            (forth (back (forth x)))))
  
  (define back-forth-type (back-forth unparse-type parse-type))
  (define back-forth-expr (back-forth unparse-expr parse-expr))
  
  (back-forth-type "asd × qwe × ert → hest")
  (back-forth-type "asd × (qwe × ert) → hest")

  (back-forth-expr "asd (qwe, λx.x x) erw")
  (back-forth-expr "asd qwe, (λx.x x) erw")
  (back-forth-expr "λp.snd p"))

