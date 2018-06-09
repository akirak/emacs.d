(require 'init-app-map)
(require 'init-hydra)

(general-def
  "<M-f1>" 'woman
  "<f5>" 'revert-buffer
  "<f6>" 'multi-term
  "<S-f6>" 'multi-term-next
  "<f7>" 'akirak/hydra-launcher-for-major-mode
  "<f8>" 'projectile-test-command
  "<f9>" 'recompile
  "<S-f9>" 'helm-make
  "<M-f10>" 'menu-bar-mode
  "<f12>" 'akirak/app-map)

;; Most of these commands are from crux and fwb-cmds.el
(general-def :prefix "<f2>"
  "D" #'crux-delete-file-and-buffer
  "o" #'crux-open-with
  "r" #'counsel-recentf
  "R" #'crux-rename-file-and-buffer
  "S" #'sudo-find-file)

(provide 'init-func-keys)
