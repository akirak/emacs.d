;; -*- no-byte-compile: t -*-
(when (version< emacs-version "25.1")
  (error "Use GNU Emacs version 25.1 or later"))

;; https://www.reddit.com/r/emacs/comments/3kqt6e/2_easy_little_known_steps_to_speed_up_emacs_start/
;; http://bling.github.io/blog/2016/01/18/why-are-you-changing-gc-cons-threshold/
;; https://github.com/kaushalmodi/.emacs.d/blob/master/init.el
(defconst modi/gc-cons-threshold--orig gc-cons-threshold)

(defsubst akirak/expand-gc-threshold ()
  (setq gc-cons-threshold most-positive-fixnum
        ;; The previous value for gc-cons-threshold.
        ;; (* 100 1024 1024)
        ))

(defsubst akirak/restore-original-gc-threshold ()
  ;; I am not sure if garbage collection should be run on minibuffer
  ;; exit events. If I remove this line, I will change the
  ;; `after-init-hook' to include garbage collection.
  (garbage-collect)
  (setq gc-cons-threshold modi/gc-cons-threshold--orig))

;; GC hack for startup
(akirak/expand-gc-threshold)

;; GC hack for minibuffers
(add-hook 'minibuffer-setup-hook #'akirak/expand-gc-threshold)
(add-hook 'minibuffer-exit-hook #'akirak/restore-original-gc-threshold)

(unless (fboundp 'whitespace-cleanup-mode)
  (defun whitespace-cleanup-mode (&rest args)
    (when (require 'whitespace-cleanup-mode nil t)
      (apply #'whitespace-cleanup-mode args))))

(add-to-list 'exec-path (expand-file-name "~/.nix-profile/bin"))

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

;; Prevent a confirmation dialog when the org file is loaded.
;; Don't forget to revert this variable at the beginning of the Org file.
(setq-default enable-local-variables :all)

(org-babel-load-file (expand-file-name "main.org" user-emacs-directory))

(run-with-idle-timer 3 nil #'akirak/restore-original-gc-threshold)
