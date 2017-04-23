#lang racket
(provide string->str
         str->string
         str-length
         str-empty?
         str-current
         gather
         skip
         skip-whites
         expect-string
         split
         splitr
         next)

(require "../opt.rkt")

(struct str (s start stop) #:transparent)

(define (string->str s)
  (str s 0 (string-length s)))

(define (str->string x)
  (match x
    [(str s start stop) (substring s start stop)]))

(define (str-length x)
  (match x
    [(str s start stop) (- stop start)]))

(define (str-empty? x)
  (= (str-length x) 0))

(define (str-current x)
  (match x
    [(str s start stop) (string-ref s start)]))
  
(define ((gather pred) x)
  (match x
    [(str s start stop)
     (define to
       (let loop ([pos start])
         (cond [(= pos stop) pos]
               [(pred (string-ref s pos)) (loop (+ pos 1))]
               [else pos])))
     (cons (substring s start to)
           (str s to stop))]))

(define (skip pred)
  (compose cdr (gather pred)))

(define skip-whites (skip char-whitespace?))

(define ((expect-string ex-s) x)
  (define len (string-length ex-s))
  (match x
    [(str s start stop)
     (cond [(> (+ start len) stop) #f]
           [else (define new-start (+ start len))
                 (and (equal? (substring s start new-start) ex-s)
                      (str s new-start stop))])]))

(define ((split char) x)
  (match x
    [(str s start stop)
     (opt>
      (let loop ([pos start] [depth 0])
        (cond [(= pos stop) #f]
              [else
               (define c (string-ref s pos))
               (cond [(and (equal? c char) (= depth 0)) pos]
                     [(and (equal? c #\)) (= depth 0)) #f]
                     [(equal? c #\)) (loop (+ pos 1) (- depth 1))]
                     [(equal? c #\() (loop (+ pos 1) (+ depth 1))]
                     [else (loop (+ pos 1) depth)])]))
     [split-pos (cons (str s start split-pos)
                      (str s (+ split-pos 1) stop))])]))

(define ((splitr char) x)
  (match x
    [(str s start stop)
     (opt>
      (let loop ([pos (- stop 1)] [depth 0])
        (cond [(= pos (- start 1)) #f]
              [else
               (define c (string-ref s pos))
               (cond [(and (equal? c char) (= depth 0)) pos]
                     [(and (equal? c #\() (= depth 0)) #f]
                     [(equal? c #\() (loop (- pos 1) (- depth 1))]
                     [(equal? c #\)) (loop (- pos 1) (+ depth 1))]
                     [else (loop (- pos 1) depth)])]))
     [split-pos (cons (str s start split-pos)
                      (str s (+ split-pos 1) stop))])]))

(define (next x)
  (match x
    [(str s start stop) (str s (+ start 1) stop)]))