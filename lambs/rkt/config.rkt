#lang racket
(provide (struct-out config)
         default-config
         update-config
         config->list
         write-config)

(struct config (img-scale) #:transparent)
(define default-config (config 3/2))

(define (update-config c x)
  (match c
    [(config old-img-scale)
     (match x
       [`(img-scale ,new-img-scale) (config new-img-scale)])]))

(define (config->list c)
  (match c
    [(config img-scale)
     `((img-scale ,img-scale))]))

(define (write-config c [out (current-output-port)])
  (for ([x (config->list c)])
    (write "\n" out)
    (write (format "\n#:config ~a\n" x))
    (write "\n" out)
    (display "\n" out)))