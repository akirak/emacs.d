;; TODO: Make this an interactive command
(defun akirak/org-insert-buffer-header (key value)
  (save-excursion
    (goto-char 1)
    ;; If the first line is part of a header, go to the next line, as the line
    ;; is likely to be a title and it is important in deft-mode.
    (when (string-match "^#\\+" (thing-at-point 'line))
      (forward-line))
    (insert "#+" key ": " value "\n")))

(provide 'akirak-org-commands)
