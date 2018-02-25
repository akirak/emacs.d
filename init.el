;;; init.el --- My main Emacs initialization file -*- no-byte-compile: t -*-

;; Copyright (C) 2018 by Akira Komamura

;; Author: Akira Komamura <akira.komamura@gmail.com>
;; Version: 0.1.0
;; Created: 13 Feb 2018
;; URL: https://github.com/akirak/emacs.d

;; This file is not part of GNU Emacs.

;;; Code:

(require 'cl-lib)

;; Check the version of Emacs
(when (version< emacs-version "25.1")
  (error "Use GNU Emacs version 25.1 or later"))

;; Load custom settings
(setq-default custom-file (expand-file-name "custom.el" user-emacs-directory))
(when (file-exists-p custom-file) (load-file custom-file))

(defcustom akirak/private-directory
  (expand-file-name "private" user-emacs-directory)
  "Directory containing private configuration files for Emacs."
  :group 'akirak)

;; Load a private custom file if it exists at ~/.emacs.d/private/custom.el
(let ((private-custom-file (expand-file-name "custom.el"
                                             akirak/private-directory)))
  (when (file-exists-p private-custom-file)
    (load-file private-custom-file)))

;; Add ~/.emacs.d/lisp to load-path
(add-to-list 'load-path (expand-file-name "lisp" user-emacs-directory))

;; Add directories in ~/.emacs.d/site-lisp to load-path
(let ((site-lisp-directory (expand-file-name "site-lisp"
                                             user-emacs-directory)))
  (when (file-directory-p site-lisp-directory)
    (dolist (dir (directory-files site-lisp-directory t
                                  'file-directory-p))
      (add-to-list 'load-path dir))))

;; Remove org-mode shipped with Emacs from load-path
(cl-delete-if (lambda (dpath) (string-match-p "/org/?$" dpath)) load-path)

;; Load packages
(let ((gc-cons-threshold most-positive-fixnum))
  ;; Load the configuration framework before loading other packages
  (require 'init-config)
  ;; Load other modules from ~/.emacs.d/modules.el
  (load-file (expand-file-name "modules.el" user-emacs-directory)))

;; Load a personal layout from ~/.emacs.d/private/init.el if the file exists
(let* ((private-directory (expand-file-name "private" user-emacs-directory))
       (private-init-file (expand-file-name "init.el" private-directory)))
  (if (file-exists-p private-init-file)
      (progn (add-to-list 'load-path private-directory)
             (load-file private-init-file))
    ;; Load the default layout otherwise
    (require 'init-default-layout)))
