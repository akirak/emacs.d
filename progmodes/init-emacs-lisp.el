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

;;;; Help
(akirak/bind-help-key :keymaps 'emacs-lisp-mode-map
  "SPC" #'akirak/emacs-lisp-hydra/body
  "m" #'emacs-lisp-macroexpand
  "s" #'suggest
  "." #'helpful-at-point)

;;;; Hydra
(defhydra akirak/emacs-lisp-hydra
  (:hint nil)
  "Emacs-Lisp"
  ("e" eval-buffer "Eval buffer" :exit t)
  ("l" package-lint-current-buffer "Package-lint" :exit t))

;;;; frame-workflow
(akirak/define-frame-workflow "emacs-lisp"
  :make-frame '(frame-purpose-make-mode-frame 'emacs-lisp-mode))

(provide 'init-emacs-lisp)
