;;; init-emacs-lisp.el --- Configuration for emacs-lisp-mode  -*- lexical-binding: t; -*-

;;;; Packages
(setq-default flycheck-emacs-lisp-load-path (quote inherit))

(use-package package-lint
  :commands (package-lint-current-buffer))

(use-package flycheck-package
  :commands (flycheck-package-setup)
  :hook
  (emacs-lisp . flycheck-package-setup))

(use-package suggest
  :commands (suggest))

;;;; Testing
(require 'init-emake)

;;;; Help
(akirak/bind-help-key :keymaps 'emacs-lisp-mode-map
  "i" #'counsel-info-lookup-symbol
  "s" #'suggest
  "." #'helpful-at-point)

;;;; Extra commands
(general-def :keymaps 'emacs-lisp-mode-map :prefix "C-z"
  "e" #'eval-buffer
  "l" #'package-lint-current-buffer
  "m" #'emacs-lisp-macroexpand)

;;;; frame-workflow
(akirak/define-frame-workflow "emacs-lisp"
  :make-frame '(frame-purpose-make-mode-frame 'emacs-lisp-mode))

(provide 'init-emacs-lisp)
