;;; -*- lexical-binding: t -*-
(require 'subr-x)

(defun s-trim-left (s)
  "Remove whitespace at the beginning of S."
  (save-match-data
    (if (string-match "\\`[ \t\n\r]+" s)
        (replace-match "" t t s)
      s)))

(defun s-trim-right (s)
  "Remove whitespace at the end of S."
  (save-match-data
    (if (string-match "[ \t\n\r]+\\'" s)
        (replace-match "" t t s)
      s)))

(defun s-trim (s)
  "Remove whitespace at the beginning and end of S."
  (s-trim-left (s-trim-right s)))


(defun get-line (n)
  (save-excursion
    (goto-line n)
    (buffer-substring-no-properties (line-beginning-position)
                                    (line-end-position))))

(defun find-empty-line (start stop step)
  (let ((res start))
    (while (and (not (= res stop)) (not (string-blank-p (get-line res))))
      (setq res (+ res step)))
    res))

(defun line-start (n)
  (save-excursion
    (goto-line n)
    (line-beginning-position)))

(defun line-stop (n)
  (save-excursion
    (goto-line n)
    (line-end-position)))

(defun get-stuff/line (line)
  (save-excursion
    (let* ((start (find-empty-line line
                                   0
                                   -1))
           (stop (find-empty-line line
                                  (line-number-at-pos (point-max))
                                  1)))
      (cons (s-trim (buffer-substring-no-properties (line-start start)
                                                    (line-stop stop)))
            (line-stop stop)))))

(defun get-stuff ()
  (get-stuff/line (line-number-at-pos)))

(defun my-read (str)
  (pcase (condition-case nil (read-from-string str) (end-of-file nil))
    ('() '())
    (`(,a . ,d) `(,a . ,(substring str d nil)))))

(setq reststring "")
(defun rpl (str)
  (setq reststring (concat reststring str))
  (let ((continue 't))
    (while continue
      (let ((res (my-read reststring)))
        (if res
            (progn
              (insert "\n")
              (pcase (car res)
                (`(img ,s)
                 (progn
                   (insert (format "(insert-pic %S)" s))
                   (insert-pic s)))
                (s (insert s)))
              (setq reststring (cdr res)))
          (setq continue '()))))))

(defun insert-pic (filename)
  (insert "\n\n\n")
  (backward-char)
  (insert-image (create-image (format "%slambs/%s" lambda-homedir filename)))
  (forward-char))
  
