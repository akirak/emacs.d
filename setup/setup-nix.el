(use-package nix-mode
  :mode "\\.nix\\'")

;; Bind nix-update-fetch to a key (I use `C-. u'), and then you can very
;; easily update the rev/sha of a fetchgit declaration.
(use-package nix-update
  :after nix-mode
  :commands (nix-update-fetch))

(use-package nixos-options :after nix-mode
  :init
  (setq akirak/nixos-options-available
        (and (bound-and-true-p nixos-options-json-file)
             (file-exists-p nixos-options-json-file))))

(use-package company-nixos-options
  :after nixos-options
  :if (bound-and-true-p akirak/nixos-options-available)
  :straight (nix-emacs :host github :repo "travisbhartwell/nix-emacs")
  :company (nix-mode . company-nixos-options))

(use-package helm-nixos-options :after (helm nixos-options)
  :if (bound-and-true-p akirak/nixos-options-available)
  :straight nix-emacs)

;; https://github.com/travisbhartwell/nix-emacs#nix-sandbox
(use-package nix-sandbox)

;; (setq flycheck-command-wrapper-function
;;       (lambda (command) (apply 'nix-shell-command (nix-current-sandbox) command))
;;       flycheck-executable-find
;;       (lambda (cmd) (nix-executable-find (nix-current-sandbox) cmd)))

(provide 'setup-nix)
