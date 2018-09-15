;;; init-emacs-lisp.el --- Configuration for emacs-lisp-mode  -*- lexical-binding: t; -*-

(add-hook 'emacs-lisp-mode-hook 'turn-on-eldoc-mode)

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

;; Generate README from Emacs Lisp.
(use-package ox-gfm
  :after ox)

(use-package el2org
  :commands (el2org-generate-readme))

;;;; Commands
(defun akirak/straight-pull-package-projectile (name)
  "Pull the package recipe for the current projectile project."
  (interactive
   (list (completing-read "Package: "
                          (--map (f-filename it)
                                 (f-directories
                                  (expand-file-name "straight/repos"
                                                    user-emacs-directory)))
                          nil t
                          (projectile-project-name))))
  (straight-pull-package (intern name))
  (when (yes-or-no-p "Rebuild the package? ")
    (straight-rebuild-package name))
  (when (yes-or-no-p "Reload it? ")
    (load-library name)))

(defun akirak/eval-buffer-or-load-file ()
  (interactive)
  (when (eq major-mode 'emacs-lisp-mode)
    (if buffer-file-name
        (load-file buffer-file-name)
      (eval-buffer))))

;;;; Testing
(require 'init-emake)

;;;; Keybindings

;;;;; Help
(akirak/bind-help-key :keymaps 'emacs-lisp-mode-map
  "i" #'counsel-info-lookup-symbol
  "s" #'suggest
  "." #'helpful-at-point)

;;;;; Extra commands
(akirak/bind-mode-key :keymaps 'emacs-lisp-mode-map
  "E" #'akirak/eval-buffer-or-load-file
  "L" #'package-lint-current-buffer
  "M" #'pp-macroexpand-last-sexp)

;;;; frame-workflow
(akirak/define-frame-workflow "emacs-lisp"
  :key "e"
  :make-frame '(frame-purpose-make-mode-frame 'emacs-lisp-mode))

(provide 'init-emacs-lisp)
