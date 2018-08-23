(require 'org-habit)
(add-hook 'org-modules 'org-protocol)

(require 'init-org-tags)
(require 'init-org-todo)

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
              org-habit-following-days 7)

(provide 'init-org-options)
