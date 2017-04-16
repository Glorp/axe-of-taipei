;;; -*- lexical-binding: t -*-

(defun findkeyword (start)
  (let ((current nil)
        (res nil)
        (done nil))
    (while (and (not done) (> start 1))
      (setq current (string (char-before start)))
      (cond ((equalp current "\\")
             (setq res start)
             (setq done 't))
            ((string-match "\\`[ \t\n\r]+" current)
             (setq done 't)))
      (setq start (- start 1)))
    res))


(setq insertstuff-list
      '((":=" . "\u225C")
        ("=>" . "\u21AA")
        ("l" . "\u03bb")
        ("L" . "\u039b")
        ("p" . "\u03C0")
        ("P" . "\u03A0")
        ("->" . "\u2192")))

(defun insertstuff ()
  (interactive)
  (let* ((stop (point))
         (start (findkeyword stop)))
    (cond (start
           (let* ((old-string (buffer-substring-no-properties start stop))
                  (new (assoc old-string insertstuff-list)))
             (cond (new
                    (delete-backward-char (+ (- stop start) 1))
                    (insert (cdr new)))))))))
