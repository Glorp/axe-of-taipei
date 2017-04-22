#lang racket
(provide repl)

(require "prove.rkt"
         "draw-proof.rkt"
         (only-in "draw.rkt" draw-text)
         "structs.rkt"
         "symbols.rkt"
         "parse.rkt"
         "unparse.rkt"
         "infer-structs.rkt"
         "slides.rkt"
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
  (write 
   (cond [(image? x) (img (scale scal x))]
         [(string? x) x]
         [else (~a x)])))

(define (repl img [scal 3/2] [draw-sym 'typey])
  ((write-stuff img scal) (format "#:scale ~a~n~n#:drawings ~a" scal draw-sym))
  (let loop ([it ":)"] [scal scal] [draw-sym draw-sym])
    (define write (write-stuff img scal))
    
    (flush-output)
    (define s (read))
    (newline)
    
    (with-handlers
        ([(λ (_) #t)
          (λ (e)
            (write (exn-message e))
            (loop it scal draw-sym))])
      
      (cond [(and (string? s) (equal? #\# (string-ref (string-trim s) 0)))
             (define in (open-input-string s))
             (match (read in)
               
               ['#:scale
                (define new-scal (read in))
                (cond [(eof-object? new-scal)
                       (write (format "#:scale ~a" scal))
                       (loop it scal draw-sym)]
                      [else
                       ((write-stuff img new-scal) (draw-text (format "images will be scaled by ~a" new-scal)))
                       (loop it new-scal draw-sym)])]
               
               ['#:drawings
                (define new-draw-sym (read in))
                (cond [(eof-object? new-draw-sym)
                       (write (format "#:drawings ~a" draw-sym))
                       (loop it scal draw-sym)]
                      [else
                       (write (format "okay :)"))
                       (loop it scal new-draw-sym)])]
               
               ['#:slide
                (for ([x (hash-ref slides (read in))])
                  (write x))
                (loop it scal draw-sym)]
               
               ['#:it
                (write it)
                (loop it scal draw-sym)]
               
               ['#:q (void)])]
            
            [else
             (write (repl1 draw-sym s))
             (loop s scal draw-sym)]))))
