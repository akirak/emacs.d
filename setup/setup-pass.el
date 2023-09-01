(use-package password-store
  :custom
  (password-store-executable "gopass"))

(use-package auth-source-pass
  :after auth-source
  :config
  (auth-source-pass-enable))

(use-package pass
  :commands (pass))

(provide 'setup-pass)
