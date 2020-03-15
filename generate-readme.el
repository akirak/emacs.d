;; -*- lexical-binding: t -*-

;; Generate README.org from init.el

(require 'package)
(setq package-archives nil)
(package-initialize)

;; Don't create a backup of README.org
(setq-default make-backup-files nil)

(require 'thingatpt)
(require 'org-make-toc)

(let* ((outfile "README.org")
       (src-buf (find-file-noselect "init.el"))
       (out-buf (create-file-buffer outfile))
       (standard-output out-buf))
  (princ "#+title: Emacs Configuration
* Table of contents
:PROPERTIES:
:TOC: siblings
:END:\n")
  (with-current-buffer src-buf
    (widen)
    (goto-char (point-min))
    (while (< (point) (point-max))
      (cond
       ((looking-at (rx eol))
        (forward-line 1))
       ((looking-at (rx ";;" (+ ";")))
        (let* ((line (thing-at-point 'line))
               (h (string-match (rx ";;" (group (+ ";")) (group (+ anything))) line))
               (level (length (match-string 1 line)))
               (heading (match-string 2 line)))
          (mapc #'princ (list (make-string level ?\*) " " heading))
          (forward-line 1)))
       ((looking-at (rx ";;"))
        (let* ((start (point))
               (end (save-excursion
                      (re-search-forward (rx bol (or ";;;" "("))
                                         nil t)
                      (1- (line-beginning-position))))
               (substr (replace-regexp-in-string
                        (rx bol (+ ";") (* space))
                        ""
                        (buffer-substring-no-properties start end))))
          (mapc #'princ (list substr "\n\n"))
          (goto-char (1+ end))))
       ((looking-at "(")
        (let* ((start (point))
               (end (progn
                      (while (looking-at (rx (* (or space "\n")) "("))
                        (forward-sexp 1))
                      (1- (point))))
               (str (buffer-substring-no-properties start end)))
          (mapc #'princ
                (list "#+begin_src emacs-lisp\n"
                      str
                      "\n#+end_src\n\n"))
          (when (re-search-forward (rx bol ";") nil t)
            (backward-char 1))))
       (t
        (error "Unexpected input: " (thing-at-point 'line))))))
  (setq standard-output t)
  (with-current-buffer out-buf
    (delay-mode-hooks (org-mode))
    (org-make-toc)
    (write-file outfile))
  (kill-buffer src-buf)
  (kill-buffer out-buf)
  (message "%s has been generated." outfile))
