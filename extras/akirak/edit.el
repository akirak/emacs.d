;;; edit.el --- synopsis -*- lexical-binding: t -*-
;;;###autoload
(defun akirak/kill-region-or-backward-kill-word (&optional arg)
  "If a region is active, run `kill-region'. Otherwise, run `backward-kill-word'."
  (interactive "p")
  (if (region-active-p)
      (kill-region (region-beginning) (region-end))
    (backward-kill-word arg)))

;;;###autoload
(defun akirak/back-to-indentation-or-beginning-of-line (&optional arg)
  (interactive "P")
  (if (or arg (akirak/at-indentation))
      (beginning-of-line)
    (back-to-indentation)))

;;;###autoload
(defun akirak/at-indentation ()
  "Check if the cursor is at the beginning of the indented line."
  (let ((col (car (posn-col-row (posn-at-point))))
        (line (thing-at-point 'line)))
    (and (not (= col 0))
         (string-match-p (rx bol (* space) eol) (substring line 0 col)))))

(provide 'akirak/edit)
;;; edit.el ends here
