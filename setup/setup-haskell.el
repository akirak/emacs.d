(use-package haskell-mode
  :mode ("\\.hs\\'" . haskell-mode)
  :config
  (defun akirak/setup-haskell-mode ()
    "Turn on either lsp-mode or dante-mode."
    (interactive)
    (unless (derived-mode-p 'haskell-mode)
      (user-error "Not haskell-mode"))
    (cond
     ((and (executable-find "ghcide")
           (require 'lsp-haskell nil t))
      (lsp))
     ((require 'dante nil t)
      (dante-mode 1)))
    (haskell-auto-insert-module-template))
  (akirak/bind-mode :keymaps 'haskell-mode-map
    "c" #'haskell-compile
    "C" (defun akirak/hpack ()
          (interactive)
          (when-let (dir (locate-dominating-file
                          default-directory "package.yaml"))
            (let ((default-directory dir))
              (shell-command "hpack ."))))
    "f" '(:wk "format")
    "fi" '(:wk "imports")
    "fii" #'haskell-mode-format-imports
    "fis" #'haskell-sort-imports
    "fia" #'haskell-align-imports
    "fs" #'haskell-mode-stylish-buffer
    "j" '(:wk "jump")
    "ji" #'haskell-navigate-imports)
  :hook
  (haskell-mode . akirak/setup-haskell-mode))

(use-package dante
  :commands dante-mode
  :config
  (defun akirak/setup-dante ()
    (flycheck-mode t)
    ;; (setq-local flymake-no-changes-timeout nil)
    ;; (setq-local flymake-start-syntax-check-on-newline nil)
    ;; (setq-local flycheck-check-syntax-automatically '(save mode-enabled))
    )
  (flycheck-add-next-checker 'haskell-dante '(info . haskell-hlint))
  (akirak/bind-mode :keymaps 'dante-mode-map
    "a" #'attrap-attrap
    "," #'dante-info
    "." #'dante-type-at)
  :general
  (:keymaps 'dante-mode-map
            "C-x C-e" #'dante-eval-block)
  :hook
  (dante-mode . akirak/setup-dante))

(use-package shm
  :after haskell-mode
  :hook
  (haskell-mode . structured-haskell-mode))

(use-package haskell-interactive-mode
  ;; I was unable to set up interactive-haskell-mode for snack.
  ;; Maybe I'll work on it later.
  :disabled t
  :commands interactive-haskell-mode
  :hook
  (haskell-mode . interactive-haskell-mode)
  :custom
  (haskell-process-suggest-remove-import-lines t)
  (haskell-process-auto-import-loaded-modules t)
  (haskell-process-log t))

(provide 'setup-haskell)
