(use-package avy
  :custom
  (avy-style 'at)
  (avy-styles-alist '((ivy-avy . pre)
                      (avy-goto-char-timer . at)
                      (akirak/avy-goto-symbol-in-window . pre)))
  (avy-keys (string-to-list "asdfghjkl")))

(use-package akirak/avy-extra
  :straight nil
  :load-path "extras"
  :init
  (akirak/bind-generic
    "is" #'akirak/insert-symbol)
  :general
  (:prefix "M-g"
           "d" #'akirak/avy-goto-defun
           "s" #'akirak/avy-goto-symbol-in-window
           "q" #'akirak/avy-goto-symbol-in-defun))

;;;; Inline jump
(defun akirak/avy-goto-in-line (regexp)
  (let (beg end)
    (save-excursion
      (beginning-of-line)
      (setq beg (point))
      (end-of-line)
      (setq end (point)))
    (avy-with avy-goto-char-in-line
      (avy--generic-jump regexp t avy-style beg end))))

(defun akirak/avy-goto-symbol-in-line ()
  (interactive)
  (akirak/avy-goto-in-line "\\_<\\sw"))

(defun akirak/avy-goto-word-in-line ()
  (interactive)
  (akirak/avy-goto-in-line "\\b\\sw"))

(defun akirak/avy-goto-quote-in-line ()
  (interactive)
  (akirak/avy-goto-in-line "\'\\S-"))

(defun akirak/avy-goto-dquote-in-line ()
  (interactive)
  (akirak/avy-goto-in-line "\"\\<"))

;;;; Jump to an open bracket in defun
(defun akirak/avy-goto-open-bracket-above-in-defun ()
  (interactive)
  (avy-with avy-goto-char
    (avy--generic-jump "[({\[]" t avy-style
                       (save-excursion (beginning-of-defun) (point))
                       (point))))

(defun akirak/avy-goto-open-bracket-below-in-defun ()
  (interactive)
  (avy-with avy-goto-char
    (avy--generic-jump "[({\[]" t avy-style
                       (1+ (point))
                       (save-excursion (end-of-defun) (point)))))

(provide 'setup-avy)
