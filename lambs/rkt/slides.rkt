#lang racket
(provide slides)

(require "rules.rkt")

(define slides
  (make-immutable-hash
   `((structural-rules . ,(list (draw-rules struct-rules)))
     (function-rules . ,(list (draw-rules fun-rules)))
     (product-rules . ,(list (draw-rules prod-rules)))
     (sum-rules . ,(list (draw-rules sum-rules))))))

