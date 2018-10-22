;; My todo state configuration for org-mode

;; The following resource provides a good example for todo configuration:
;; - http://doc.norang.ca/org-mode.html#TodoKeywords

;;;; Utilities

(defun akirak//org-todo-keyword-names ()
  (cl-loop for (type . group) in org-todo-keywords
           when (eq type 'sequence)
           append (cl-loop for spec in group
                           unless (equal spec "|")
                           collect (replace-regexp-in-string "(.+)$" "" spec))))

;;;; Default configuration
(setq-default org-todo-keywords
              '((sequence
                 "TODO(t)"
                 "NEXT(n!)"
                 "STARTED(s!)"
                 "REVIEW(r!)" ; I probably need to review my task after working on it
                 "|"
                 "DONE(d)")
                (sequence
                 "MAYBE(m@)"
                 ;; Probably deprecated soon
                 "BLOCKED(b@/!)"
                 "WAITING(w@/!)" ; Waiting for a particular starting time.
                 "URGENT(u!/)"
                 "|"
                 "ARCHIVED(a@/!)")
                (type
                 "TO_BLOG(l)"
                 "HABIT(h)"
                 ;; Define these tags precisely
                 "TOPIC(o)"
                 "FIX(f)"
                 "IDEA(i)")))

(setq-default org-todo-state-tags-triggers
              (append '(("ARCHIVED" ("ARCHIVE" . t)))
                      (mapcar (lambda (kw)
                                `(,kw (,(concat "@" (downcase kw)) . t)))
                              '("FIX" "TOPIC"))))

;; Based on the dracula color scheme. See https://github.com/dracula/dracula-theme/
(setq org-todo-keyword-faces
      `(("TODO" . (:foreground "SpringGreen2" :weight bold))
        ("NEXT" . (:foreground "yellow2" :weight bold))
        ("STARTED" . (:foreground "DarkOrange" :weight bold))
        ;; Warning
        ("URGENT" . (:foreground "red"))
        ("TOPIC" . (:foreground "LightSeaGreen" :weight bold))
        ("FIX" . (:foreground "VioletRed4" :weight bold))
        ;; Review and to_blog: italicized
        ("REVIEW" . (:foreground "orange1" :slant italic))
        ("TO_BLOG" . (:foreground "LightGoldenrod" :slant italic :weight bold))
        ;; Done-like states
        ("DONE" . (:foreground "ForestGreen"))
        ("ARCHIVED" . (:foreground "DarkGrey" :underline t))
        ;; Deprecated, but similar to ARCHIVED
        ("CANCELLED" . (:foreground "DarkGrey" :underline t))
        ;; Inactive states
        ("BLOCKED" . (:foreground "IndianRed1" :weight bold :underline t))
        ("WAITING" . (:foreground "MediumPurple2" :weight bold :underline t))
        ("MAYBE" . (:foreground "LimeGreen" :underline t))))

(ert-deftest akirak/org-todo-keyword-faces-defined ()
  "`org-todo-keyword-faces' should be defined for each item in `org-todo-keywords'."
  (cl-loop for keyword in (akirak//org-todo-keyword-names)
           do (should (assoc keyword org-todo-keyword-faces))))

(defun akirak/clock-in-to-next (kw)
  "Switch a task from TODO to IN_PROGRESS when clocking in.
Skips capture tasks, projects, and subprojects."
  (when (and (not (and (boundp 'org-capture-mode) org-capture-mode))
             (not (equal (org-entry-get nil "STYLE") "habit")))
    (cond
     ((member (org-get-todo-state) (list "TODO" "NEXT" "WAITING"))
      "STARTED"))))
(setq-default org-clock-in-switch-to-state #'akirak/clock-in-to-next)

(provide 'init-org-todo)
