;;; init.el --- My main Emacs initialization file -*- no-byte-compile: t -*-

(require 'cl-lib)

;; Check the version of Emacs
(when (version< emacs-version "25.1")
  (error "Use GNU Emacs version 25.1 or later"))

;; TODO: Try this to accelerate the startup time
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
                  "coding"
                  "misc"
                  "org"
                  "apps"
                  "exwm"
                  "ui"
                  "international"))
  (add-to-list 'load-path (expand-file-name subdir akirak/dotemacs-directory)))

;; Load custom settings
(setq-default custom-file (expand-file-name "custom.el" akirak/dotemacs-directory))
(when (file-exists-p custom-file) (load-file custom-file))

;; This needs to be loaded first
(require 'init-config)

;;; Basic configuration
(require 'init-defaults)
(require 'init-keybindings)
(require 'init-org)
(require 'init-ui)
(require 'init-helm)
(require 'init-ivy)
(require 'init-coding)

;;; Enhancements (misc/)
(require 'init-commands)
(require 'init-startup)                 ; Optional
(require 'init-desktop)
(require 'init-midnight)
(require 'init-playground)
(require 'init-locate)

;;; Apps (apps/)
(require 'init-multi-term)
(require 'init-prodigy)
(require 'init-dired)
(require 'init-linux-disk)
(require 'init-magit)
(require 'init-ibuffer)
(require 'init-docker)

;;; Programming languages (progmodes/)
(require 'init-graphviz)
(require 'init-nim)
(require 'init-lispy)
(require 'init-markdown)
(require 'init-haskell)
(require 'init-dockerfile)

;;; Natural languages (international/)
(require 'init-japanese)
