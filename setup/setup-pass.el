(use-package pass
  :commands (pass))

(use-package password-store
  :preface
  (setq password-store-executable
        ;; gopass claims to be 100%-compatible with pass
        (or (executable-find "gopass")
            ;; fallback to pass
            (executable-find "pass")))
  :if (or password-store-executable
          (progn
            (message "gopass or pass is not installed, so password-store won't be available"))
          nil))

(use-package auth-source-pass
  :config
  (auth-source-pass-enable))

;; TODO: Add an Ivy interface which wraps gopass

(provide 'setup-pass)
