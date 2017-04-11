#lang racket
(provide repl)

(require "img.rkt"
         "prove.rkt"
         "draw.rkt"
         "structs.rkt"
         "parse.rkt"
         "unparse.rkt"
         "infer-structs.rkt")

(define curdir (current-directory))
(define dir (path->string (build-path curdir "img")))
(make-directory* dir)

(define img (make-image-cacher dir))

(define (relative-path from to)
  (define res (path->string (find-relative-path (path-only from) to)))
  (string-replace res  "\\" "/"))


(define (string->ty/exp-strings s)
  (match (string-split s "\n")
    ['() #f]
    [(list _) #f]
    [(list ty exps ...)
     (cons ty (string-join exps "\n"))]))

(define (repl1 s)
  (with-handlers
    ([(λ (_) #t) (λ (e) (exn-message e))])
    (match (string->ty/exp-strings s)
      [(cons ty exp)
       (match (parse-type ty)
         [#f (~a "could not parse type: " ty)]
         [ty (match (parse-expr exp)
               [#f (~a "could not parse expression: " exp)]
               [exp (match (prove (: exp ty))
                      [#f "nope"]
                      [res (img (draw (infer-map sequent->typestring res)))])])])]
      [#f (~a "bad input: " s)])))

(define (repl)
  (write "repl repl :)")
  (flush-output)
  (let loop ()
    (define s (read))
    (newline)
    (write (repl1 s))
    (flush-output)

    (loop)))
