#lang racket
(provide make-image-cacher)

(require file/convertible)

(define (save-tmpimage imgbytes i fulldir dir)
  (define filename (format "img~a.png" i))
  (define path (build-path fulldir filename))
  (with-output-to-file path #:exists 'truncate
    (lambda () (display imgbytes)))
  `(img ,(format "~a/~a" dir filename)))

(define (cache-img img cache fulldir dir)
  (hash-ref! cache
             img
             (λ ()
               (save-tmpimage (convert img 'png-bytes)
                              (length (hash-keys cache))
                              fulldir
                              dir))))

(define (image-value value cache fulldir dir)
  (and (convertible? value)
       (cache-img value cache fulldir dir)))

(define (make-image-cacher root dir)
  (define fulldir (path->string (build-path root dir)))
  (make-directory* fulldir)
  (define cache (make-hash))
  (λ (img)
    (image-value img cache fulldir dir)))

