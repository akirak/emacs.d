;; This library provides the following commands:
;;
;; - A variant of =org-schedule= and =org-agenda-schedule= which accept a numeric prefix N and schedule the entry in N days.
;;
;; Corresponding keybindings are remapped to the commands.
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

(general-def :keymaps 'org-mode-map :package 'org
  [remap org-schedule] #'akirak/org-schedule)
(general-def :keymaps 'org-agenda-mode-map :package 'org
  [remap org-agenda-schedule] #'akirak/org-agenda-schedule)

(provide 'setup-org-custom-commands)
