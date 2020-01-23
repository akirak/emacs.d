(use-package nix-mode
  :mode "\\.nix\\'"
  :config
  (akirak/bind-mode :keymaps 'nix-mode-map
    "u" #'nix-update-fetch))

(use-package nix-buffer
  :commands (nix-buffer))

(use-package helm-nixos-options
  :after (nixos-options)
  :straight (:host github :repo "travisbhartwell/nix-emacs"))

(use-package nix-update
  :commands (nix-update-fetch))

(use-package nix-sandbox)

(use-package nix-env-install
  :config
  (akirak/bind-admin
    "n" '(nil :wk "nix")
    "nC" #'nix-env-install-cachix-use
    "nu" #'nix-env-install-uninstall))

(provide 'setup-nix)
