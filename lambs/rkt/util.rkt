#lang racket
(provide >>)

(define (>> . fs)
  (apply compose (reverse fs)))