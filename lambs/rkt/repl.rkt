#lang racket
(provide repl)

(require "prove.rkt"
         "config.rkt"
         "draw-proof.rkt"
         "structs.rkt"
         "symbols.rkt"
         "parse.rkt"
         "unparse.rkt"
         "infer-structs.rkt"
         (only-in 2htdp/image scale image?))

(define (string->ty/exp-strings s)
  (match (string-split s "\n")
    ['() #f]
    [(list _) #f]
    [(list ty exps ...)
     (cons ty (string-join exps "\n"))]))

(define (draw sym p)
  (match sym
    ['typey (draw-proof-typey p)]
    ['termy (draw-proof-termy p)]
    ['logicy (draw-proof-logicy p)]
    [x (format "dunno how do draw thing ~a-like like..?" x)]))

(define (repl1 draw-sym s)
  (match (string->ty/exp-strings s)
    [(cons ty exp)
     (match (parse-type ty)
       [#f (~a "could not parse type: " ty)]
       [ty (match (parse-expr exp)
             [#f (~a "could not parse expression: " exp)]
             [exp (match (prove (: exp ty))
                    [#f "nope"]
                    [res (draw draw-sym res)])])])]
    [#f (~a "bad input: " s)]))

(define ((write-stuff img scal) x)
  (define i (img x))
  (write 
   (cond [(image? x) (img (scale scal x))]
         [(string? x) x]
         [else (~a x)])))

(define (repl img [c default-config])
  (write-config c)
  (let loop ([it ":)"] [c c])
    (match c
      [(config scal draw-sym)
       (define write (write-stuff img scal))
       
       (flush-output)
       (define s (read))
       (newline)
       
       (with-handlers
           ([(λ (_) #t)
             (λ (e)
               (write (exn-message e))
               (loop it c))])
         
         (cond [(and (string? s) (equal? #\# (string-ref (string-trim s) 0)))
                (define in (open-input-string s))
                (match (read in)
                  ['#:config
                   (define x (read in))
                   (cond [(eof-object? x)
                          (write-config c)
                          (loop it c)]
                         [else
                          (define new-config (sexpr->config x))
                          (write "okay :)")
                          (loop it new-config)])]
                  ['#:it
                   (write it)
                   (loop it c)]
                  ['#:q (void)])]
               
               [else
                (write (repl1 draw-sym s))
                (loop s c)]))])))
