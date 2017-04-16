;;; -*- lexical-binding: t -*-
(require 'cl-lib)
(require 'cl)

(horizontal-scroll-bar-mode 1)

(global-set-key (kbd "M-\\") 'insertstuff)

(defun get-string-from-file (filePath)
  "Return filePath's file content."
  (with-temp-buffer
    (insert-file-contents filePath)
    (buffer-string)))

(setq lambrackmode 'racket)

(setq lambdabuffer "lambda")

(defun lambd-print-string (str)
  (pcase lambrackmode
    ('racket (insert str))
    ('stuff (rpl str))))

(defun exec-lamb ()
  (interactive)
  (pcase (get-stuff)
    (`() (progn (goto-char (line-end-position))
                (insert "\nnope :(")))
    (`(,s . ,pos) (progn
                    (goto-char pos)
                    (if (> (current-column) 0) (insert "\n"))
                    (process-send-string "lambdproc" (format "%S\n" s))))))

(defun stop-lamb ()
  (interactive)
  (kill-process "lambdproc"))

(defun start-stuff ()
  (setq lambrackmode 'stuff)
  (setq reststring "")
  (insert "\n")
  (process-send-string "lambdproc" "(begin (require \"rkt/repl.rkt\") (repl))\n"))

(defun start-lamb ()

  (rename-buffer "lambda")
  (setq lambrackmode 'racket)
  (setq reststring "")
  (setq lambdabuffer "lambda")

  (start-process "lambdproc" (current-buffer) "racket")
  (set-process-filter (get-process "lambdproc")
                      (lambda (p s)
                        (let ((v (get-buffer-window lambdabuffer)))
                          (if v
                              (with-selected-window v
                                (lambd-print-string s))
                            (with-current-buffer (get-buffer lambdabuffer)
                              (lambd-print-string s))))))
  (global-set-key (kbd "C-e") 'exec-lamb)
  (insert "\n"))
