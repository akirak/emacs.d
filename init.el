;;; init.el --- My main Emacs initialization file -*- no-byte-compile: t -*-

(require 'cl-lib)

;; Check the version of Emacs
(when (version< emacs-version "25.1")
  (error "Use GNU Emacs version 25.1 or later"))

;; Set gc-cons-threshold temporarily to accelerate startup time
(setq gc-cons-threshold (* 50 1000 1000))
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
                  "hydra"
                  "misc"
                  "org"
                  "apps"
                  "exwm"
                  "ui"
                  "international"
                  "x"))
  (add-to-list 'load-path (expand-file-name subdir akirak/dotemacs-directory)))

;; This needs to be loaded first
(require 'init-compat)
(require 'init-config)

(use-package hydra)
(use-package ov
  :straight (ov :host github :repo "ShingoFukuyama/ov.el"))

;;; Basic configuration
(require 'init-defaults)
(require 'init-keybindings)
(require 'init-org)
(require 'init-ui)
(require 'init-helm)
(require 'init-ivy)
(require 'init-coding)

;;; Enhancements (misc/)
(require 'init-savehist)
(require 'init-commands)
(require 'init-startup)                 ; Optional
;; (require 'init-desktop)
(require 'init-midnight)
(require 'init-locate)
(require 'init-gpastel)
;; (require 'init-autosave)
(require 'init-super-save)
(require 'init-browse-url)
(require 'init-repom)
(require 'init-chrome)
(require 'init-git-auto-commit)
(require 'init-purgatory)

;;; Apps (apps/, x/)
(require 'init-terminal)
(require 'init-prodigy)
(require 'init-dired)
(require 'init-linux-disk)
(require 'init-magit)
(require 'init-ibuffer)
(require 'init-docker)
(require 'init-web-search)
(require 'init-web-browser)
(require 'init-pocket-reader)
;; (require 'init-eaf)

;;; Programming languages (progmodes/)
(require 'init-git-modes)
(require 'init-graphviz)
(require 'init-python)
(require 'init-nim)
(require 'init-lispy)
(require 'init-markdown)
(require 'init-haskell)
(require 'init-dockerfile)
(require 'init-kotlin)
(require 'init-emacs-lisp)
(require 'init-shell-scripts)
(require 'init-yaml)
(require 'init-beancount)
(require 'init-nix)

;;; Natural languages (international/)
(require 'init-japanese)
;; (require 'init-chinese)
(require 'init-google-translate)

;;; Personal configuration (local/)
(defcustom akirak/use-personal-configuration nil
  "When non-nil, load the personal configuration.

The personal configuration resides in \"local\" directory of this
repository.  Files prefixed with \"local-\" are automatically loaded."
  :type 'boolean
  :group 'akirak
  :set (lambda (symbol value)
         (set symbol value)
         (when value
           (add-to-list 'load-path (expand-file-name "local" user-emacs-directory))
           (require 'init-local))))
