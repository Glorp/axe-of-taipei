#lang racket
(provide type-hash
         logic-hash)

(define type-hash
  #hasheq([hypo . "H"]
          [weak . "W"]
          [? . "?"]
          [prod . "×"]
          [sum . "+"]
          [fun . "→"]))

(define logic-hash
  #hasheq([hypo . "H"]
          [weak . "W"]
          [? . "?"]
          [prod . "∧"]
          [sum . "∨"]
          [fun . "⊃"]))