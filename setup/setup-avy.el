(use-package avy)

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

;;;; Jump to the beginning of defun
(defun akirak/avy-goto-defun (beg end)
  (avy-with avy-goto-line
    (avy--generic-jump (concat "^(\\|"
                               (mapconcat (lambda (l)
                                            (concat "\\(" (nth 1 l) "\\)"))
                                          imenu-generic-expression
                                          "\\|"))
                       t avy-style beg end)))

(defun akirak/avy-goto-defun-above ()
  (interactive)
  (akirak/avy-goto-defun (window-start) (point))
  (back-to-indentation))

(defun akirak/avy-goto-defun-below ()
  (interactive)
  (akirak/avy-goto-defun (1+ (point)) (window-end))
  (back-to-indentation))

(provide 'setup-avy)
