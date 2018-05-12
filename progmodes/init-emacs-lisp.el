;;; init-emacs-lisp.el --- Configuration for emacs-lisp-mode  -*- lexical-binding: t; -*-

(setq-default flycheck-emacs-lisp-load-path (quote inherit))

(use-package package-lint
  :commands (package-lint-current-buffer))

(use-package flycheck-package
  :commands (flycheck-package-setup))

(use-package helpful
  :commands (helpful-function
             helpful-variable
             helpful-at-point
             helpful-command)
  :general
  ([help ?f] 'helpful-function
   [help ?v] 'helpful-variable
   [help ?\.] 'helpful-at-point))

(define-prefix-command 'akirak/emacs-lisp-extra-map)

(general-def akirak/emacs-lisp-extra-map
  "e" 'eval-buffer
  "l" 'package-lint-current-buffer
  "Cp" 'flycheck-package-setup)

(provide 'init-emacs-lisp)
