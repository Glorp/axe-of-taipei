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

(defun start-lamb (imgdir)

  (setq reststring "")

  (start-process "lambdproc" (current-buffer) "racket" "repl.rkt" imgdir)
  (set-process-filter (get-process "lambdproc")
                      (lambda (p s)
                        (rpl s)))
  (global-set-key (kbd "C-e") 'exec-lamb)
  (global-set-key (kbd "<M-up>")
                  (lambda ()
                    (interactive)
                    (process-send-string "lambdproc" (format "%S\n" "#:it"))))
  (insert "\n"))

