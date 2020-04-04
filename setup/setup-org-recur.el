(use-package org-recur
  :commands (org-recur-mode org-recur-agenda-mode)
  :general
  (:keymaps 'org-recur-mode-map :package 'org-recur
            [remap org-todo]
            (general-predicate-dispatch #'org-todo
              (akirak/org-recur-heading-p) #'akirak/org-recur-state))
  (:keymaps 'org-agenda-mode-map :package 'org-agenda
            [remap org-agenda-todo]
            (general-predicate-dispatch #'org-agenda-todo
              (akirak/org-recur-heading-p) #'akirak/org-agenda-recur-state))
  :config
  (defmacro akirak/org-agenda-with-entry (&rest progn)
    `(when-let ((marker (or (org-get-at-bol 'org-marker)
                            (org-get-at-bol 'org-hd-marker))))
       (with-current-buffer (marker-buffer marker)
         (goto-char marker)
         ,@progn)))
  (defun akirak/org-recur-heading-p ()
    (cond
     ((derived-mode-p 'org-mode)
      (org-recur--get-next-date (org-get-heading t t t t)))
     ((derived-mode-p 'org-agenda-mode)
      (akirak/org-agenda-with-entry
       (akirak/org-recur-heading-p)))))
  (defun akirak/org-recur-state ()
    (interactive)
    (cl-ecase (read-char-choice "Choose an action [d: done, e: edit schedule]: "
                                (string-to-list "de"))
      (?d (org-recur-finish))
      (?e (akirak/org-recur-set))))
  (defun akirak/org-agenda-recur-state ()
    (interactive)
    (akirak/org-agenda-with-entry
     (akirak/org-recur-state)))
  (advice-add 'org-recur-finish
              :after
              (defun akirak/org-recur-revert-done-subtasks (&rest _args)
                (save-excursion
                  (let ((pos (point))
                        (end (save-excursion (org-end-of-subtree))))
                    (org-map-entries
                     (lambda ()
                       (when (and (org-entry-is-done-p)
                                  (member "@recur" (org-get-tags)))
                         (org-todo 'none)))
                     nil 'tree)))))
  (defun akirak/org-recur-set ()
    (interactive)
    (let* ((date-desc (completing-read "Schedule: "
                                       '("+2 / recur every other day"
                                         "+w / recur every week"
                                         "1 / recur on the first day of every month"
                                         "Thu / recur every Thursday"
                                         "Sun,Sat / recur every Sunday and Saturday"
                                         "Wkdy / recur every weekday"
                                         "1 10:00, 15 12:00 / recur at particular time on particular date")))
           (date (if (string-match (rx bol (group (+? anything)) "/") date-desc)
                     (string-trim-right (match-string 1 date-desc))
                   date-desc))
           (cookie (format "|%s|" date))
           (orig-headline (org-get-heading t t t t))
           (headline (concat cookie " "
                             (if (string-match (concat "^" "\\(" org-recur--regexp "\\)") orig-headline)
                                 (substring orig-headline (1+ (match-end 1)))
                               orig-headline))))
      (org-edit-headline headline)))
  (add-hook 'org-after-todo-state-change-hook
            (defun akirak/org-recur-maybe-setup ()
              (when (equal (org-get-todo-state) "RECUR")
                (unless (bound-and-true-p org-recur-mode)
                  (when (yes-or-no-p "org-recur-mode is inactive. Turn on it?")
                    (org-recur-mode 1)
                    (when (yes-or-no-p "Add to the file header?")
                      (add-file-local-variable-prop-line 'mode 'org-recur))))
                (unless (akirak/org-recur-heading-p)
                  (akirak/org-recur-set))
                (unless (org-entry-get nil "SCHEDULED")
                  (org-recur-schedule-date (akirak/org-recur-heading-p)))))))

(provide 'setup-org-recur)
