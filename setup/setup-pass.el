(defconst akirak/password-store-directory
  (let ((dir (or (getenv "password_store_dir")
                 "~/.password-store")))
    (when (file-directory-p dir)
      dir)))

(use-package pass
  :commands (pass))

(use-package password-store
  :preface
  (setq password-store-executable
        ;; gopass claims to be 100%-compatible with pass
        (or (executable-find "gopass")
            ;; fallback to pass
            (executable-find "pass")))
  :if (and (or password-store-executable
               (progn
                 (message "gopass or pass is not installed, so password-store won't be available"))
               nil)))

(use-package auth-source-pass
  :if (and akirak/password-store-directory
           (file-directory-p akirak/password-store-directory))
  :config
  (auth-source-pass-enable))

;; TODO: Add an Ivy interface which wraps gopass

(provide 'setup-pass)
