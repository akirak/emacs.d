(use-package nix-mode
  :mode "\\.nix\\'")

(use-package nixos-options :after nix-mode
  :init
  (setq akirak/nixos-options-available
        (and (bound-and-true-p nixos-options-json-file)
             (file-exists-p nixos-options-json-file))))

(use-package company-nixos-options :after (company nixos-options)
  :if (bound-and-true-p akirak/nixos-options-available)
  :straight (nix-emacs :host github :repo "travisbhartwell/nix-emacs")
  :init
  (add-to-list 'company-backends 'company-nixos-options t))

(use-package helm-nixos-options :after (helm nixos-options)
  :if (bound-and-true-p akirak/nixos-options-available)
  :straight nix-emacs)

(provide 'init-nix)
