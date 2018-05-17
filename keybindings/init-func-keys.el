(require 'init-app-map)
(require 'init-project-map)
(require 'init-hydra)

(general-def
  "<S-f1>" 'helm-apropos
  "<M-f1>" 'woman
  "<f5>" 'revert-buffer
  "<f6>" 'multi-term
  "<f7>" 'akirak/hydra-launcher-for-major-mode
  "<f8>" 'projectile-test-command
  "<S-f8>" 'akirak/project-map
  "<f9>" 'recompile
  "<S-f9>" 'helm-make
  "<f12>" 'akirak/app-map
  "<S-f12>" 'helm-org-starter
  "<M-f12>" 'helm-org-rifle
  "<M-S-f12>" 'org-journal-search)

;; Most of these commands are from crux and fwb-cmds.el
(general-def :prefix "<f2>"
  "D" #'crux-delete-file-and-buffer
  "F" #'switch-to-current-buffer-other-frame
  "k" #'kill-this-buffer-and-its-window
  "o" #'crux-open-with
  "r" #'counsel-recentf
  "R" #'crux-rename-file-and-buffer
  "S" #'sudo-find-file)

(provide 'init-func-keys)
