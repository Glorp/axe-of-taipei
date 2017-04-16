#lang racket
(provide (struct-out config)
         default-config
         sexpr->config
         config->sexpr
         write-config)

(struct config (scale foo) #:transparent)
(define default-config (config 3/2 'typey))

(define (sexpr->config x)
  (match x
    [`((scale ,scale)
       (foo ,foo))
     (config scale foo)]))

(define (config->sexpr c)
  (match c
    [(config scale foo)
     `((scale ,scale)
       (foo ,foo))]))

(define (write-config c [out (current-output-port)])
  (write "\n" out)
  (write (format "\n#:config ~a\n" (config->sexpr c)))
  (write "\n" out)
  (display "\n" out))