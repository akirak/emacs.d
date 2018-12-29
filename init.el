;; -*- no-byte-compile: t -*-
(when (version< emacs-version "25.1")
  (error "Use GNU Emacs version 25.1 or later"))

;; https://www.reddit.com/r/emacs/comments/3kqt6e/2_easy_little_known_steps_to_speed_up_emacs_start/
;; https://github.com/kaushalmodi/.emacs.d/blob/master/init.el
(defvar modi/gc-cons-threshold--orig gc-cons-threshold)
(setq gc-cons-threshold (* 100 1024 1024)) ;100 MB before garbage collection

;;;; Configure straight.el
(load-file (expand-file-name "core/straight.el" user-emacs-directory))

;; Install use-package using straight.el
(straight-use-package 'use-package)

;; Use straight.el by default in use-package directives
(setq straight-use-package-by-default t)

;;;; Use the latest Git version of Org mode
(require 'cl-lib)
(require 'subr-x)

;; Remove org-mode shipped with Emacs from load-path
(cl-delete-if (lambda (dpath) (string-match-p "/org/?$" dpath)) load-path)

;; Install org-mode from the Git repository
(load-file (expand-file-name "core/org-from-git.el" user-emacs-directory))

;;;; Load configuration files
(load-file (expand-file-name "core/setup.el" user-emacs-directory))
(add-to-list 'load-path (expand-file-name "extras" user-emacs-directory))
(org-babel-load-file (expand-file-name "main.org" user-emacs-directory))
;; Load my personal config
(let ((file "~/learning/toolbox.org"))
  (when (file-exists-p file)
    (org-babel-load-file file t)))

;;;; Finalization
;; https://github.com/kaushalmodi/.emacs.d/blob/master/init.el
(when modi/gc-cons-threshold--orig
  (run-with-idle-timer 5 nil (lambda () (setq gc-cons-threshold modi/gc-cons-threshold--orig))))
