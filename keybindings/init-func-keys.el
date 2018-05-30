(require 'init-app-map)
(require 'init-project-map)
(require 'init-hydra)
(require 'init-global-org-map)

(general-def
  "<S-f1>" 'helm-apropos
  "<M-f1>" 'woman
  "<S-f2>" #'akirak/global-org-map
  "<f5>" 'revert-buffer
  "<f6>" 'multi-term
  "<S-f6>" 'multi-term-next
  "<f7>" 'akirak/hydra-launcher-for-major-mode
  "<f8>" 'projectile-test-command
  "<S-f8>" 'akirak/project-map
  "<f9>" 'recompile
  "<S-f9>" 'helm-make
  "<M-f10>" 'menu-bar-mode
  "<f12>" 'akirak/app-map)

;; Most of these commands are from crux and fwb-cmds.el
(general-def :prefix "<f2>"
  "c" #'magit-branch-and-checkout
  "D" #'crux-delete-file-and-buffer
  "F" #'switch-to-current-buffer-other-frame
  "k" #'kill-this-buffer-and-its-window
  "m" (lambda () (interactive) (magit-checkout "master"))
  "o" #'crux-open-with
  "r" #'counsel-recentf
  "R" #'crux-rename-file-and-buffer
  "S" #'sudo-find-file)

(provide 'init-func-keys)
