(defvar akirak/org-mode-hydra-outline-width 80)

(defun akirak/org-mode-hydra--generate-path ()
  (--> (org-get-outline-path t nil)
       (org-format-outline-path it 240 nil " > ")
       (substring-no-properties it)
       (let* ((len (length it))
              (p akirak/org-mode-hydra-outline-width)
              (n (+ (/ len p) (if (= 0 (mod len p)) 0 1))))
         (mapcar (lambda (i) (substring it (* i p) (min len (* (1+ i) p))))
                 (number-sequence 0 (1- n))))
       (string-join it "\n ")))

(defun akirak/org-agenda-hydra-title ()
  (akirak/org-with-maybe-agenda-origin
   (string-join `(,(akirak/org-mode-hydra--generate-path)
                  " ------------------------------------- "
                  ,(format " Created at %s, total clocked %s"
                           (org-entry-get nil "CREATED_TIME")
                           (org-duration-from-minutes (org-clock-sum-current-item)))
                  ,(format " Custom ID: %s,  ID: %s"
                           (org-entry-get nil "CUSTOM_ID")
                           (org-entry-get nil "ID"))
                  ,@(let ((trigger (org-entry-get nil "TRIGGER"))
                          (blocker (org-entry-get nil "BLOCKER")))
                      (when (or trigger blocker)
                        (delq nil
                              (list
                               (when trigger (format " Trigger: %s" trigger))
                               (when blocker (format " Blocker: %s" blocker)))))))
                "\n")))

(major-mode-hydra-define org-mode
  (:title (akirak/org-agenda-hydra-title)
          :foreign-keys t)
  ("Store link to this entry"
   (("li" (progn
            (org-id-get-create)
            (call-interactively 'org-store-link))
     "With ID")
    ("lc" (progn
            (akirak/org-set-custom-id-property)
            (call-interactively 'org-store-link))
     "With custom ID"))
   "Mark/config"
   (("mc" (unless (org-entry-get nil "CREATED_TIME")
            (org-entry-put nil "CREATED_TIME"
                           ;; TODO: Check for clock data in the entry
                           (format-time-string (org-time-stamp-format t t))))
     "Set created time")
    ("%" (akirak/org-add-statistics-cookie "%") "Add [%%%%]")
    ("/" (akirak/org-add-statistics-cookie "/") "Add [/]")
    ("ms" org-sidebar-tree)
    ;; TODO: Show history
    )
   "Set"
   (("sc" akirak/org-set-category "Category")
    ("st" org-edit-headline "Headline"))
   "Relationship"
   (("e" org-linker-edna))))

(major-mode-hydra-define org-agenda-mode
  (:title (akirak/org-agenda-hydra-title)
          :foreign-keys t)
  ("Store link to this entry"
   (("li" (progn
            (org-id-get-create)
            (call-interactively 'org-store-link))
     "With ID")
    ("lc" (progn
            (akirak/org-set-custom-id-property)
            (call-interactively 'org-store-link))
     "With custom ID"))
   "Set"
   (("sc" akirak/org-set-category "Category"))))

;;;; org-habit support

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

;;;; Adding a statistics cookie

(defun akirak/org-add-statistics-cookie (type)
  (save-excursion
    (if (org-at-heading-p)
        (progn
          (org-back-to-heading)
          (re-search-forward org-complex-heading-regexp))
      (re-search-backward org-complex-heading-regexp))
    (let* ((start (match-beginning 4))
           (end (match-end 4))
           (heading (buffer-substring start end))
           (placeholder (format " [%s]" type)))
      (if (string-match (rx (* (any space))
                            "["
                            (or (and (* digit) "%")
                                (and (* digit) "/" (* digit)))
                            "]"
                            (* (any space)))
                        heading)
          (replace-string (match-string 0 heading) placeholder nil start end)
        (goto-char end)
        (insert placeholder)))
    (org-update-statistics-cookies nil)))

;;;; Commands

(defun akirak/org-set-property-interactively (propname)
  (akirak/org-with-maybe-agenda-origin
   (org-set-property propname nil))
  (when (derived-mode-p 'org-agenda-mode)
    (org-agenda-redo)))

(defun akirak/org-set-category ()
  (interactive)
  (akirak/org-set-property-interactively "CATEGORY"))

(provide 'setup-org-hydra)
