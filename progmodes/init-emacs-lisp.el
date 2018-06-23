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
(defun akirak/setup-emake ()
  (interactive)
  (let ((project-root (projectile-project-root))
        (package-basename (read-from-minibuffer "Package basename: "
                                                (when (eq major-mode 'emacs-lisp-mode)
                                                  (file-name-base (buffer-file-name)))))
        (filenames '("Makefile" ".travis.yml" ".gitignore")))
    (dolist (filename filenames)
      (unless (or (not (file-exists-p (expand-file-name filename project-root)))
                  (yes-or-no-p (format "%s already exists. Overwrite it?" filename)))
        (user-error "Aborted")))
    (dolist (filename filenames)
      (let ((fpath (expand-file-name filename project-root))
            (url (concat "https://raw.githubusercontent.com/vermiculus/emake.el-example/master/" filename)))
        (url-copy-file url fpath 'overwrite)))
    (find-file (expand-file-name "Makefile" project-root))
    (goto-char (point-min))
    (re-search-forward (rx bol "PACKAGE_BASENAME" (1+ space)
                           ":=" (1+ space) (group (1+ wordchar)) eol))
    (replace-match package-basename nil nil nil 1)
    (re-search-forward (rx "wget"))
    (replace-match "curl -O")))

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
