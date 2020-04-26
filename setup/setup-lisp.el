;;; setup-lisp.el --- Configuration for lisp-mode  -*- lexical-binding: t; -*-

(general-add-hook '(lisp-mode-hook
                    emacs-lisp-mode-hook)
                  '(turn-on-eldoc-mode))
(general-add-hook '(lisp-mode-hook
                    emacs-lisp-mode-hook)
                  '(rainbow-delimiters-mode-enable))

(setq-mode-local emacs-lisp-mode
                 imenu-generic-expression
                 `(("Headings" ";;[;]\\{1,8\\} \\(.*$\\)" 1)
                   (nil "^\\s-*(\\(cl-def\\(?:generic\\|ine-compiler-macro\\|m\\(?:acro\\|ethod\\)\\|subst\\|un\\)\\|def\\(?:advice\\|generic\\|ine-\\(?:advice\\|compil\\(?:ation-mode\\|er-macro\\)\\|derived-mode\\|g\\(?:\\(?:eneric\\|lobal\\(?:\\(?:ized\\)?-minor\\)\\)-mode\\)\\|inline\\|m\\(?:ethod-combination\\|inor-mode\\|odify-macro\\)\\|s\\(?:etf-expander\\|keleton\\)\\)\\|m\\(?:acro\\|ethod\\)\\|s\\(?:etf\\|ubst\\)\\|un\\*?\\)\\|ert-deftest\\)\\s-+\\(\\(?:\\sw\\|\\s_\\|\\\\.\\)+\\)" 2)
                   ("Custom vars" "^\\s-*(\\(defcustom\\)\\s-+\\(\\(?:\\sw\\|\\s_\\|\\\\.\\)+\\)" 2)
                   ("Variables" "^\\s-*(\\(def\\(?:c\\(?:onst\\(?:ant\\)?\\|ustom\\)\\|ine-symbol-macro\\|parameter\\)\\)\\s-+\\(\\(?:\\sw\\|\\s_\\|\\\\.\\)+\\)" 2)
                   ("Variables" "^\\s-*(defvar\\(?:-local\\)?\\s-+\\(\\(?:\\sw\\|\\s_\\|\\\\.\\)+\\)[[:space:]\n]+[^)]" 1)
                   ("Types" "^\\s-*(\\(cl-def\\(?:struct\\|type\\)\\|def\\(?:class\\|face\\|group\\|ine-\\(?:condition\\|error\\|widget\\)\\|package\\|struct\\|t\\(?:\\(?:hem\\|yp\\)e\\)\\)\\)\\s-+'?\\(\\(?:\\sw\\|\\s_\\|\\\\.\\)+\\)" 2)
                   ;; Only in my configuration
                   ("use-package" ,(rx "(use-package" (+ space)
                                       (group (+ (any alnum "-"))))
                    1)
                   ("org-capture" ,(rx "(org-starter-def-capture" (+ space)
                                       (+ (not space)) (+ space)
                                       (syntax string-quote)
                                       (group (+ (not (syntax string-quote)))))
                    1)))

;;;; Packages
(setq-default flycheck-emacs-lisp-load-path (quote inherit))

(use-package package-lint
  :commands (package-lint-current-buffer))

(use-package buttercup)

(use-package package-requires
  :straight (package-requires :host github
                              :repo "akirak/package-requires.el"))

(use-package flycheck-package
  :commands (flycheck-package-setup))

(use-package suggest
  :commands (suggest))

(use-package el2org
  :disabled t
  :commands (el2org-generate-readme))

(use-package eros
  :config
  ;; TODO: Add an advice for lispy-eval
  (eros-mode 1))

(use-package nameless
  :commands (nameless-mode)
  :disabled t
  :general
  (:keymaps 'nameless-mode-map
            "-" 'nameless-insert-name-or-self-insert))

(use-package elx
  :straight (elx :host github :repo "emacscollective/elx"))

(use-package lisp-extra-font-lock)

;;;; Package editing
(defun akirak/emacs-lisp-setup-package ()
  (let* ((filename (buffer-file-name))
         (filename-nondir (and filename (file-name-nondirectory filename)))
         (dir (and filename (abbreviate-file-name (file-name-directory filename)))))
    (when (and filename
               (or (string-prefix-p (concat user-emacs-directory "straight/") dir)
                   (not (or (string-prefix-p user-emacs-directory dir)
                            (equal "~/" dir)
                            (member filename-nondir '(".dir-locals.el"))
                            (string-match-p "-tests?\\.el\\'" filename)))))
      (flycheck-package-setup)
      (flycheck-mode 1)
      (when (fboundp 'nameless-mode)
        (nameless-mode 1)))))

(add-hook 'emacs-lisp-mode-hook #'akirak/emacs-lisp-setup-package)

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

(provide 'setup-lisp)
