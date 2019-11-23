(use-package nix-mode
  :mode "\\.nix\\'"
  :config
  (akirak/bind-mode :keymaps 'nix-mode-map
    "u" #'nix-update-fetch))

(use-package helm-nixos-options
  :after (nixos-options)
  :straight (:host github :repo "travisbhartwell/nix-emacs"))

(use-package nix-update
  :commands (nix-update-fetch))

(use-package nix-sandbox)

(provide 'setup-nix)
