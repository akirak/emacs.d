(require 'org-habit)
(add-hook 'org-modules 'org-protocol)

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
              org-tags-exclude-from-inheritance '())

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
  "M-n" 'org-metadown
  "M-p" 'org-metaup
  "M-H" 'org-shiftmetaleft
  "M-L" 'org-shiftmetaright
  "C-1" 'counsel-org-tag
  "C-8" 'org-insert-hydra/body
  "C-9" #'org-tree-to-indirect-buffer)

(provide 'setup-org)
