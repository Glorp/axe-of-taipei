#lang racket
(provide parse-type)

(require "../structs.rkt"
         "../opt.rkt"
         "str.rkt"
         "util.rkt")

(struct funt () #:transparent)
(struct prodt () #:transparent)
(struct sumt () #:transparent)

(define (prodify l)
  (opt>
   (split-list prodt? l)
   [prod-split
    (opt>
     (opt-map singleton-first prod-split)
     [res
      (left-assoc prod res)])]))

(define (sumify l)
  (opt>
   (split-list sumt? l)
   [sum-split
    (opt>
     (opt-map prodify sum-split)
     [res
      (left-assoc sum res)])]))

(define (parse-type x)
  (define res
    (opt>
     (read-type-list x)   
     [type-l
      (opt>
       (split-list funt? type-l)
       [fun-split
        (opt>
         (opt-map sumify fun-split)
         [funs (right-assoc fun funs)])])]))
  (and (type? res) res))

(define (read-type-list y)
  (define ((add-type x) rest-str)
    (opt> (read-type-list rest-str)
          [rest (cons x rest)]))
          
  (define (halp y)
    (define x (skip-whites y))
    (cond [(str-empty? x) '()]

          [(equal? (str-current x) #\()
           (opt> ((split #\)) (next x))
                 [(cons inside rest)
                  (opts>
                   ((parse-type inside) (halp rest))
                   [(a d) (cons a d)])])]
          
          [((expect-string "->") x) => (add-type (funt))]
          [((expect-string "→") x) => (add-type (funt))]

          [((expect-string "*") x) => (add-type (prodt))]
          [((expect-string "×") x) => (add-type (prodt))]
          
          [((expect-string "+") x) => (add-type (sumt))]
          
          [else (opt>
                 (read-word x)
                 [(cons w rest)
                  (opt>
                   (halp rest)
                   [rest-list (cons (ty (string->symbol w)) rest-list)])])]))
  (match (halp y)
    ['() #f]
    [x x]))

