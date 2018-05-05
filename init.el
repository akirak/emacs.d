;;; init.el --- My main Emacs initialization file -*- no-byte-compile: t -*-

(require 'cl-lib)

;; Check the version of Emacs
(when (version< emacs-version "25.1")
  (error "Use GNU Emacs version 25.1 or later"))

;; (setq gc-cons-threshold (* 50 1000 1000))
;; (setq gc-cons-threshold most-positive-fixnum)
(add-hook 'emacs-startup-hook (lambda () (setq gc-cons-threshold 800000)))

;; Remove org-mode shipped with Emacs from load-path
(cl-delete-if (lambda (dpath) (string-match-p "/org/?$" dpath)) load-path)

(defcustom akirak/dotemacs-directory user-emacs-directory
  "The directory where this directory exists."
  :group 'akirak)

;; Add directories for init files
(dolist (subdir '("core"
                  "keybindings"
                  "progmodes"
                  "tools"
                  "misc"
                  "org"
                  "apps"
                  "exwm"
                  "ui"
                  "international"))
  (add-to-list 'load-path (expand-file-name subdir akirak/dotemacs-directory)))

;; Load custom settings
(let ((fpath (expand-file-name "~/ops/custom.el")))
  (setq-default custom-file (if (file-exists-p fpath)
                                fpath
                              (expand-file-name "custom.el" user-emacs-directory))))
(when (file-exists-p custom-file) (load-file custom-file))

;; This needs to be loaded first
(require 'init-config)

(require 'init-defaults)
(require 'init-startup)                 ; Optional
(require 'init-shell-bindings)

(require 'init-dracula-theme)
(require 'init-visual-cues)

