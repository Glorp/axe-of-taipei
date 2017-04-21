#lang racket

(module+ main
  (require "rkt/repl.rkt"
           "rkt/img.rkt")
  
  (define img-dir
    (match (current-command-line-arguments)
      [(vector) "temp"]
      [(vector x) x]))
  
  (define img (make-image-cacher (current-directory) img-dir))
  
  (repl img))
