;;; init-org-agenda-bindings.el --- Keybindings for org-agenda-mode -*- lexical-binding: t -*-

;; This should be compatible with init-org-bindings.el as much as possible

(general-def :keymaps 'org-agenda-mode-map :package 'org
  "C-1" 'counsel-org-tag
  "C-4" 'org-refile-hydra)

(provide 'init-org-agenda-bindings)
;;; init-org-agenda-bindings.el ends here
