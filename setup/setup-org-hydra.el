(defvar-local akirak/org-allow-habits nil
  "Allow creating habit entries if this variable is non-nil.")

(defun akirak/org-habit-scheduled-p ()
  (let ((schedule (org-entry-get nil "SCHEDULED")))
    (and (stringp schedule)
         (string-match-p (rx ".+") schedule))))

(defun akirak/org-set-habit ()
  (interactive)
  (unless (derived-mode-p 'org-mode)
    (user-error "Not in org-mode"))
  (unless akirak/org-allow-habits
    (user-error "Habits are not allowed in this file"))
  (ignore-errors
    (org-todo "HABIT"))
  (org-set-property "STYLE" "habit")
  (if (akirak/org-habit-scheduled-p)
      (message "Already set a habit schedule")
    (let ((start (org-read-date nil t))
          (interval (read-string "Interval (e.g. \"1w\"): ")))
      (save-excursion
        (org-back-to-heading t)
        (if (re-search-forward org-scheduled-regexp
                               (save-excursion (org-end-of-subtree))
                               t)
            (delete-region (line-beginning-position) (line-end-position))
          (beginning-of-line 2)
          (insert "\n")
          (beginning-of-line 0))
        (insert (s-replace ">" (format " .+%s>" interval)
                           (org-format-time-string (concat "SCHEDULED: "
                                                           (org-time-stamp-format))
                                                   start)))))))

(defun akirak/org-batch-set-habit ()
  (interactive)
  (unless (derived-mode-p 'org-mode)
    (user-error "Not in org-mode"))
  (goto-char (point-min))
  (while (re-search-forward (rx bol (+ "*") (+ space) "HABIT") nil t)
    (akirak/org-set-habit)))

(major-mode-hydra-define org-mode
  (:title "Org" :foreign-keys t)
  ("Type"
   (("th" akirak/org-set-habit "Set habit"))))

(provide 'setup-org-hydra)
