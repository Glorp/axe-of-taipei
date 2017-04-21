#lang racket
(provide repl)

(require "img.rkt"
         "prove.rkt"
         "config.rkt"
         "draw.rkt"
         "structs.rkt"
         "symbols.rkt"
         "parse.rkt"
         "unparse.rkt"
         "infer-structs.rkt"
         2htdp/image)

(define (relative-path from to)
  (define res (path->string (find-relative-path (path-only from) to)))
  (string-replace res  "\\" "/"))


(define (string->ty/exp-strings s)
  (match (string-split s "\n")
    ['() #f]
    [(list _) #f]
    [(list ty exps ...)
     (cons ty (string-join exps "\n"))]))

(define (repl1 s c img)
  (match c
    [(config scal f)
     (define my-draw
       (match f
         ['typey (λ (x) (draw (infer-map (draw-coloured-sequent sequent->typestring)
                                         draw-rule
                                         x)))]
         ['termy (λ (x) (draw (infer-map (draw-coloured-sequent sequent->termstring)
                                         draw-rule
                                         x)))]
         ['logicy (λ (x) (draw (infer-map (draw-coloured-sequent (λ (s) (sequent->typestring s logic-hash)))
                                          (λ (r) (draw-rule r logic-hash))
                                          x)))]
         [x 'welp]))
     (match (string->ty/exp-strings s)
       [(cons ty exp)
        (match (parse-type ty)
          [#f (~a "could not parse type: " ty)]
          [ty (match (parse-expr exp)
                [#f (~a "could not parse expression: " exp)]
                [exp (match (prove (: exp ty))
                       [#f "nope"]
                       [res (img (scale scal (my-draw res)))])])])]
       [#f (~a "bad input: " s)])]))

(define (repl img [c default-config])
  (write-config c)
  (let loop ([it ":)"] [c c])
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
           (write (repl1 s c img))
           (loop s c)]))))

(module+ main

  (define img-dir
    (match (current-command-line-arguments)
      [(vector) "temp"]
      [(vector x) x]))
  (define img (make-image-cacher (current-directory) img-dir))

  (repl img))