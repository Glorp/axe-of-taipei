#lang racket
(provide draw-proof
         draw-rule
         draw-coloured-sequent
         draw-text)

(require "infer-structs.rkt"
         "symbols.rkt"
         2htdp/image)

(define (draw-rule r [h type-hash] [col 'black])
  (define (halp name num)
    (define rimg (txt name col))
    (match num
      [#f rimg]
      [n (define iimg (txt n col 14))
         (overlay/xy rimg (image-width rimg) 10 iimg)]))

  (match r
    [#f (halp " " #f)]
    [(rule s num) (halp (hash-ref h s) num)]
    [(intro s num) (halp (~a (hash-ref h s) "I") num)]
    [(elim s num) (halp (~a (hash-ref h s) "E") num)]))

(define (draw-text s [col 'black])
  (beside (whitespace 2) (txt s col) (whitespace 2)))

(define ((draw-coloured-sequent sequent->string) x)
  (match x
    [(coloured s c)
     (draw-text (sequent->string s) c)]))

(define (draw-proof i)
  (car (draw-inf-halp i)))

(define (txt x col [size 20])
  (text/font x
             size
             col
             #f
             'roman
             'normal
             'normal
             #f))

(define (whitespace w)
  (rectangle w 20 'solid (color 0 0 0 0)))

(define (besidel l)
  (match l
    ['() (whitespace 10)]
    [(list x) x]
    [l (apply beside/align 'bottom l)]))

(define (draw-inf-halp i)
  (match i
    
    [(inference c-img r-img ps) 
     (match (map draw-inf-halp ps)
       [(list (list p-imgs p-insetls p-insetrs) ...)
        (define p-img (besidel (add-between p-imgs (whitespace 15))))
        (define p-insetl (if (null? p-insetls) 0 (first p-insetls)))
        (define p-insetr (if (null? p-insetrs) 0 (last p-insetrs)))
        (define p-insetlength (- (image-width p-img) p-insetl p-insetr))        
        (define c-start0 (+ (/ (- p-insetlength (image-width c-img)) 2) p-insetl))
        (define c-start (if (< c-start0 0) 0 c-start0))
        (define p-start (if (< c-start0 0) (- c-start0) 0))
        (match (if (> p-insetlength (image-width c-img))
                   (cons (+ p-start p-insetl) p-insetlength)
                   (cons c-start (image-width c-img)))
          [(cons line-start line-length)
           (define line-end (+ line-start line-length))
           (define empty (rectangle (max (+ p-start (image-width p-img))
                                         (+ line-end (image-width r-img) 4)
                                         (+ c-start (image-width c-img)))
                                    (+ (image-height c-img) (image-height p-img) 6)
                                    'solid
                                    (color 0 0 0 0)))
           (define (place-img arg scene)
             (match arg
               [(list img x y) (place-image/align img x y 'left 'top scene)]))
           (define prem/conc (foldl place-img
                                  empty
                                  (list (list p-img p-start 0)
                                        (list c-img c-start (+ (image-height p-img) 6)))))
           (define prem/conc/rul (if r-img
                                     (foldl place-img
                                            prem/conc
                                            (list (list r-img (+ line-end 4) (+ (image-height p-img) 2 (- (/ (image-height r-img) 2))))
                                                  (list (line line-length 0 'black) line-start (+ (image-height p-img) 3))))
                                     prem/conc))
           (list prem/conc/rul c-start (- (image-width empty) (+ c-start (image-width c-img))))])])]
    
    [img (list img 0 0)]))


(module+ main
  (draw-proof (inference (beside (circle 10 'solid 'red)
                           (circle 10 'solid 'black)
                           (circle 10 'solid 'red))
                   (circle 5 'solid 'red)
                   (list (triangle 20 'solid 'blue)
                         (inference (square 15 'solid 'yellow)
                                    (flip-vertical (square 7 'solid 'yellow))
                                    (list (ellipse 20 10 'solid 'green)
                                          (star 17 'solid 'gray)))))))
    
