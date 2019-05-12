(require 'org-habit)
(add-hook 'org-modules 'org-protocol)

(when (bound-and-true-p akirak/mode-prefix-key)
  (general-translate-key nil 'org-mode-map
    :package 'org akirak/mode-prefix-key "C-c C-x"))

(setq-default org-clock-history-length 20
              org-clock-mode-line-total (quote today)
              org-clock-out-remove-zero-time-clocks t
              org-clock-persist t
              org-clock-persist-query-resume nil
              org-enforce-todo-dependencies t
              org-log-done (quote time)
              org-log-into-drawer t
              org-log-refile nil
              org-outline-path-complete-in-steps nil
              org-refile-allow-creating-parent-nodes (quote confirm)
              org-refile-use-outline-path (quote full-file-path)
              org-src-tab-acts-natively t
              org-startup-indented t
              org-startup-truncated nil
              org-use-speed-commands t
              org-habit-graph-column 1
              org-habit-preceding-days 21
              org-habit-following-days 7
              org-id-link-to-org-use-id 'create-if-interactive-and-no-custom-id
              org-group-tags t
              ;; org-use-fast-tag-selection t
              ;; org-fast-tag-selection-single-key nil
              org-agenda-use-tag-inheritance t
              org-tags-exclude-from-inheritance '()
              org-special-ctrl-a/e t
              org-M-RET-may-split-line nil)

;; https://yiufung.net/post/org-mode-hidden-gems-pt1/
(setq-default org-cycle-separator-lines 0
              org-catch-invisible-edits 'show-and-error)

;; Prevent from saving org-refile and org-capture locations to bookmarks
(setq org-bookmark-names-plist nil)

(setq org-clock-goto-may-find-recent-task nil)

;; https://emacs.stackexchange.com/questions/21171/company-mode-completion-for-org-keywords
(defun org-add-completion-at-point ()
  (add-hook 'completion-at-point-functions 'pcomplete-completions-at-point
            nil t))
(add-hook 'org-mode-hook #'org-add-completion-at-point)

(with-eval-after-load 'org-clock
  (org-clock-persistence-insinuate))

(general-def :keymaps 'org-mode-map :package 'org
  ;; I don't use any of these bindings and want to use them for other purposes
  "C-c [" nil
  "C-c ]" nil
  ;; M-up/down/left/right is unavailable on Chromebooks, so I need
  ;; alternative bindings for commands bound on those keys.
  "M-n" 'org-metadown
  "M-p" 'org-metaup
  "M-H" 'org-shiftmetaleft
  "M-L" 'org-shiftmetaright
  "C-1" 'counsel-org-tag)

(akirak/bind-mode :keymaps 'org-mode-map :package 'org
  "t" 'akirak/org-table-create-or-edit)

(defun akirak/org-table-create-or-edit ()
  (interactive)
  (if (org-at-table-p)
      (akirak/org-table-hydra/body)
    (org-table-create)))

(defhydra akirak/org-table-hydra (:hint nil)
  "
Org Table

        ^^Insert  ^^Delete
Row     _ir_      _dr_
Column  _ic_      _dc_

Edit: _e_
Navigation: _n_ _p_ _f_ _b_
"
  ("dc" org-table-delete-column)
  ("dr" org-table-kill-row)
  ("ic" org-table-insert-column)
  ("ir" org-table-insert-row)
  ("e" org-edit-special)
  ("f" org-table-next-field)
  ("b" org-table-previous-field)
  ("n" org-table-next-row)
  ("p" previous-line))

(defun akirak/ad-around-org-beginning-of-line (orig n)
  (cond
   ((org-at-property-p)
    (if (bolp)
        (when-let* ((line (thing-at-point 'line))
                    (match (string-match org-property-re line)))
          (beginning-of-line 1)
          (right-char (string-match (regexp-quote (match-string 3 line))
                                    line)))
      (beginning-of-line n)))
   (t (funcall orig n))))
(advice-add 'org-beginning-of-line :around
            'akirak/ad-around-org-beginning-of-line)

(defmacro akirak/org-define-set-property-command (property)
  (let ((command (intern (format "akirak/org-set-%s-property"
                                 (downcase (s-replace "_" "-" property))))))
    `(defun ,command ()
       ,(format "Set %s property of the subtree." property)
       (interactive)
       (cl-case current-prefix-arg
         ('(4) (princ (org-entry-get nil ,property t)))
         (otherwise (org-set-property ,property nil))))))

(add-to-list 'which-key-replacement-alist
             '(("C-," . "akirak/org-set-") .
               (lambda (kb)
                 (if (string-match (rx bol "akirak/org-set-"
                                       (group (+ any))
                                       "-property" eol)
                                   (cdr kb))
                     (cons (car kb) (match-string 1 (cdr kb)))
                   kb))))

(akirak/org-define-set-property-command "CUSTOM_ID")

(akirak/bind-mode :keymaps 'org-mode-map :package 'org
  "h" '(nil :wk "heading")
  "he" '(org-edit-headline :wk "edit")
  "hs" '(org-insert-subheading :wk "ins subheading")
  "hS" '(org-insert-todo-subheading :wk "ins todo subheading")
  "hr" '(org-insert-heading-respect-content :wk "ins respect")
  "hR" '(org-insert-todo-heading-respect-content :wk "ins todo respect")
  "M-p" '(nil :wk "common props")
  "M-p c" 'akirak/org-set-custom-id-property
  "M-p i" 'org-id-get-create)

(general-def :keymaps 'org-read-date-minibuffer-local-map
  "C-p" (lambda () (interactive)
          (org-eval-in-calendar '(calendar-forward-week -1)))
  "C-n" (lambda () (interactive)
          (org-eval-in-calendar '(calendar-forward-week 1)))
  "<tab>" (lambda () (interactive)
            (org-eval-in-calendar '(calendar-forward-day 1)))
  "<S-iso-lefttab>" (lambda () (interactive)
                      (org-eval-in-calendar '(calendar-forward-day -1))))

(defun akirak/org-set-created-timestamp (&rest args)
  "Add a creation timestamp to the current Org entry.

If the current command is run with a prefix argument, prevent
from running."
  (unless current-prefix-arg
    (org-set-property "CREATED_TIME"
                      (org-timestamp-format
                       (org-timestamp-from-time (current-time) t t)
                       (org-time-stamp-format t t)))))

(advice-add #'org-insert-heading
            :after #'akirak/org-set-created-timestamp)

(provide 'setup-org)
