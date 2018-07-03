(defun akirak/org-insert-buffer-header (key value)
  (interactive
   (list (read-string "Key: ")
         (read-string "Value: ")))
  (org-with-wide-buffer
   (goto-char (point-min))
   ;; If the first line is part of a header, go to the next line, as the line
   ;; is likely to be a title and it is important in deft-mode.
   (when (string-match "^#\\+" (thing-at-point 'line))
     (forward-line))
   (insert "#+" key ": " value "\n")
   ;; Return the value
   value))

(provide 'akirak-org-commands)
