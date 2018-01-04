;;; init.el --- Main initialization file of Emacs -*- no-byte-compile: t -*-

;; Copyright (C) 2018 by Akira Komamura

;; Author: Akira Komamura <akira.komamura@gmail.com>
;; Version: 0.1.0
;; URL: https://github.com/akirak/emacs.d

;;; Code:

;; Check the version of Emacs
(when (version< emacs-version "24.4")
  (error "Use GNU Emacs version 24.4 or later"))

;; Bootstrap straight.el package manager
;; See https://github.com/raxod502/straight.el#getting-started for details
(let ((bootstrap-file (concat user-emacs-directory "straight/repos/straight.el/bootstrap.el"))
      (bootstrap-version 3))
  (unless (file-exists-p bootstrap-file)
    (with-current-buffer
        (url-retrieve-synchronously
         "https://raw.githubusercontent.com/raxod502/straight.el/develop/install.el"
         'silent 'inhibit-cookies)
      (goto-char (point-max))
      (eval-print-last-sexp)))
  (load bootstrap-file nil 'nomessage))

;; This is required in order to use `use-package`
(straight-use-package 'use-package)

(require 'cl)
(require 'subr-x)

(setq custom-file (expand-file-name "custom.el" user-emacs-directory))

(setq init-directory (expand-file-name "init" user-emacs-directory))

(defcustom init-file-blacklist '() "Init files to ignore."
  :group 'akirak :type '(repeat symbol))

(defun reload-emacs ()
  (interactive)
  (cl-loop for fname in (directory-files init-directory nil "\.el$")
           unless (memq (intern (file-name-base fname)) init-file-blacklist)
           do (load-file (expand-file-name fname init-directory))))

(reload-emacs)
