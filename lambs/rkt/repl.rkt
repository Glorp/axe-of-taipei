#lang racket
(provide repl)

(require "img.rkt"
         "prove.rkt"
         "config.rkt"
         "draw.rkt"
         "structs.rkt"
         "parse.rkt"
         "unparse.rkt"
         "infer-structs.rkt"
         2htdp/image)

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

(define (repl1 s c)
  (match c
    [(config scal f)
     (define foo
       (match f
         ['typey sequent->typestring]
         ['termy sequent->termstring]
         [x 'welp]))
     (match (string->ty/exp-strings s)
       [(cons ty exp)
        (match (parse-type ty)
          [#f (~a "could not parse type: " ty)]
          [ty (match (parse-expr exp)
                [#f (~a "could not parse expression: " exp)]
                [exp (match (prove (: exp ty))
                       [#f "nope"]
                       [res (img (scale scal (draw
                                              (infer-map (draw-coloured-sequent foo)
                                                         draw-rule
                                                         res))))])])])]
      [#f (~a "bad input: " s)])]))

(define (repl [c default-config])
  (write "repl repl :)")
  (write-config c)
  (let loop ([c c])
    (flush-output)
    (define s (read))
    (newline)

    (with-handlers
        ([(λ (_) #t)
          (λ (e)
            (write (exn-message e))
            (loop c))])
      
      (cond [(and (string? s) (equal? #\# (string-ref (string-trim s) 0)))
             (define in (open-input-string s))
             (match* ((read in) (read in))
               [('#:config x)
                (define new-config (sexpr->config x))
                (write "\nokay :)")
                (loop new-config)])]
                
          [else
           (write (repl1 s c))
           (loop c)]))))
