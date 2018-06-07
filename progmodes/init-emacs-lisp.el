;;; init-emacs-lisp.el --- Configuration for emacs-lisp-mode  -*- lexical-binding: t; -*-

(setq-default flycheck-emacs-lisp-load-path (quote inherit))

(use-package package-lint
  :commands (package-lint-current-buffer))

(use-package flycheck-package
  :commands (flycheck-package-setup))

(define-prefix-command 'akirak/emacs-lisp-extra-map)

(general-def akirak/emacs-lisp-extra-map
  "e" 'eval-buffer
  "l" 'package-lint-current-buffer
  "Cp" 'flycheck-package-setup)

(akirak/define-frame-workflow "emacs-lisp"
  :make-frame '(frame-purpose-make-mode-frame 'emacs-lisp-mode))

(provide 'init-emacs-lisp)
