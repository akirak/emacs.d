;; -*- no-byte-compile: t -*-

(dolist (subdir '("keybindings"
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
  (add-to-list 'load-path (expand-file-name subdir user-emacs-directory)))

;;; Basic configuration

(require 'init-keybindings)
(require 'init-org)
(require 'init-ui)
(require 'init-helm)
(require 'init-coding)

;;; Enhancements (misc/)
(require 'init-savehist)
(require 'init-commands)
(require 'init-startup)                 ; Optional
;; (require 'init-desktop)
(require 'init-midnight)
(require 'init-locate)
;; (require 'init-gpastel)
(require 'init-clipmon)
;; (require 'init-autosave)
(require 'init-super-save)
(require 'init-browse-url)
;; (require 'init-repom)
(require 'init-chrome)
(require 'init-git-auto-commit)
(require 'init-forge)
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
(require 'init-elixir)
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
