;; -*- no-byte-compile: t -*-

(dolist (subdir '("progmodes"
                  "coding"
                  "hydra"
                  "misc"
                  "org"
                  "apps"
                  "ui"
                  "international"
                  "x"))
  (add-to-list 'load-path (expand-file-name subdir user-emacs-directory)))

;;; Basic configuration

(require 'init-org)
(require 'init-coding)

;;; Enhancements (misc/)
;; (require 'init-repom)
(require 'init-purgatory)
