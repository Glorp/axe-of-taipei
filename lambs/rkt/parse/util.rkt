#lang racket
(provide left-assoc
         right-assoc
         split-list
         read-word
         singleton-first)

(require "str.rkt")
         
(define (left-assoc constr l)
  (match l
    [(list x xs ...) (foldl (λ (a b) (constr b a)) x xs)]))


(define (right-assoc constr l)
  (match l
    [(list xs ... x) (foldl constr x (reverse xs))]))

(define (split-list pred lst)
  (match lst
    ['() #f]
    [_ (define res
         (let loop ([l lst] [res '()] [current '()])
           (match l
             ['() (cons (reverse current) res)]
             [(list x xs ...) (cond [(not (pred x)) (loop xs res (cons x current))]
                                    [(null? current) #f]
                                    [else (loop xs (cons (reverse current) res) '())])])))
       (and res
            (reverse res))]))

(define delims
  '(#\( #\) #\. #\, #\λ #\: #\× #\→ #\- #\= #\> #\* #\+))

(define (id-char? c)
  (and (not (char-whitespace? c))
       (not (member c delims))))

(define (read-word x)
  (define res ((compose (gather id-char?) skip-whites) x))
  (and (not (equal? (car res) ""))
       res))

(define (singleton-first s)
  (match s
    [(list x) x]
    [(list xs ...) #f]))