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

(defun akirak/org-schedule (arg)
  "`org-schedule' which allows specifying a relative date as a numeric prefix."
  (interactive "P")
  (if (integerp arg)
      (org-schedule nil (akirak/org-relative-date-string arg))
    (org-schedule arg)))

(defun akirak/org-agenda-schedule (arg)
  "`org-agenda-schedule' which allows specifying a relative date as a numeric prefix."
  (interactive "P")
  (if (integerp arg)
      (org-agenda-schedule nil (akirak/org-relative-date-string arg))
    (org-agenda-schedule arg)))

(defun akirak/org-relative-date-string (n)
  (format-time-string "%F" (+ (float-time) (* n 86400))))

(provide 'akirak-org-commands)
