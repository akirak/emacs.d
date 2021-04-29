(use-package smartparens
  :config
  (require 'smartparens-config)
  (defun akirak/setup-smartparens-mode ()
    (interactive)
    (electric-pair-local-mode -1)
    (unless (bound-and-true-p polymode-mode)
      (when (derived-mode-p 'prog-mode 'json-mode 'sgml-mode)
        (turn-on-smartparens-strict-mode))))
  :hook
  (after-change-major-mode . akirak/setup-smartparens-mode)
  (minibuffer-setup . turn-on-smartparens-strict-mode))

(provide 'setup-smartparens)
