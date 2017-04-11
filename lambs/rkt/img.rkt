#lang racket
(provide make-image-cacher)

(require file/convertible)

(define (save-tmpimage imgbytes i dir)
  (define filename (format "img~a.png" i))
  (define path (build-path dir filename))
  (with-output-to-file path #:exists 'truncate
    (lambda () (display imgbytes)))
  `(img ,filename))

(define (cache-img img cache dir)
  (hash-ref! cache
             img
             (λ ()
               (save-tmpimage (convert img 'png-bytes)
                              (length (hash-keys cache))
                              dir))))

(define (image-value value cache dir)
  (and (convertible? value)
       (cache-img value cache dir)))

(define (make-image-cacher dir)
  (define cache (make-hash))
  (λ (img)
    (image-value img cache dir)))

