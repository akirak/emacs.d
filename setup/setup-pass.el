(use-package pass
  :commands (pass))

(use-package password-store
  :preface
  (setq password-store-executable
        ;; gopass claims to be 100%-compatible with pass
        (or (executable-find "gopass")
            ;; fallback to pass
            (executable-find "pass")))
  :if (if password-store-executable
          t
        (user-error "gopass or pass is not installed")
        nil)
  :ensure-system-package
  ((gopass . "nix-env -i gopass")))

(use-package auth-source-pass
  :config
  (auth-source-pass-enable))

;; TODO: Add an Ivy interface which wraps gopass

(provide 'setup-pass)
