(use-package org-mind-map
  :after (org ox)
  :config
  ;; org-do-wrap does not seem to be defined in the current version
  ;; of org-mode. Stolen from the following place:
  ;; https://github.com/emacs-china/org-mode/blob/maint/lisp/org.el
  (unless (fboundp 'org-do-wrap)
    (defun org-do-wrap (words width)
      "Create lines of maximum width WIDTH (in characters) from word list WORDS."
      (let (lines line)
        (while words
          (setq line (pop words))
          (while (and words (< (+ (length line) (length (car words))) width))
            (setq line (concat line " " (pop words))))
          (setq lines (push line lines)))
        (nreverse lines)))))

(provide 'init-org-mind-map)
