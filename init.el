;; -*- lexical-binding: t -*-
;;; Prerequisites
;; - Linux or Window Subsystem for Linux
;; - Nix package manager for installing dependencies

;;; Installation
;; This configuration depends on some external programs as well as Emacs
;; Lisp packages built by Nix.

;; At present, a suggested installation procedure is to first install my [[https://github.com/akirak/home.nix][home.nix]] and then run =mr checkout= at your home directory.
;; In the future, it may support an alternative for trying out.

;;; License
;; GPL v3.

;; Some libraries were originally written by other people, and they
;; follow their respective licenses.

;;; Initialization

(when (version< emacs-version "25.1")
  (error "Use GNU Emacs version 25.1 or later"))

;; Expand the GC threshold until gcmh-mode is activated.
;; gcmh-mode updates this value later, so you don't have to reset it.
;; The value is stolen from http://akrl.sdf.org/
(setq gc-cons-threshold #x40000000)

(unless (fboundp 'whitespace-cleanup-mode)
  (defun whitespace-cleanup-mode (&rest args)
    (when (require 'whitespace-cleanup-mode nil t)
      (apply #'whitespace-cleanup-mode args))))

(add-to-list 'exec-path (expand-file-name "~/.nix-profile/bin"))

(defconst akirak/to-be-run-as-exwm (member "--exwm" command-line-args))

(defun akirak/exwm-session-p ()
  akirak/to-be-run-as-exwm)

;;;; Configure straight.el
(load-file (expand-file-name "core/straight.el" user-emacs-directory))

;; Install use-package using straight.el
(straight-use-package 'use-package)

;; Use straight.el by default in use-package directives
(setq straight-use-package-by-default t)

;;;; Benchmarking the startup process
(use-package benchmark-init
  :hook
  (after-init . benchmark-init/deactivate))

;;;; Use the latest Git version of Org mode
(require 'cl-lib)
(require 'subr-x)

;; Remove org-mode shipped with Emacs from load-path
(cl-delete-if (lambda (dpath) (string-match-p "/org/?$" dpath)) load-path)

;; Install org-mode from the Git repository
(load-file (expand-file-name "core/org-from-git.el" user-emacs-directory))
;;;; Recipe overrides
(mapc #'straight-use-package
      '((org-web-tools :host github :repo "akirak/org-web-tools"
                       :branch "encoding")))

;;;; Load configuration files

(add-to-list 'load-path (expand-file-name "lisp" user-emacs-directory))
(require 'my/const/system)

;;;; Migrating
;; TODO: Move to lisp/
(add-to-list 'load-path (expand-file-name "extras" user-emacs-directory))

;; Prevent a confirmation dialog when the org file is loaded.
;; Don't forget to revert this variable at the beginning of the Org file.
(setq-default enable-local-variables :all)
(load-file (expand-file-name "core/setup.el" user-emacs-directory))
(org-babel-load-file (expand-file-name "main.org" user-emacs-directory))

;;; Packages
(use-package docker)
(use-package org-recent-headings
  :after org
  :config
  (org-recent-headings-mode 1))

;;; Autoloads
(use-package my/project
  :straight (:type built-in))

(use-package my/buffer/predicate
  :straight (:type built-in))

;;; Commands and keybindings
;;;; Switching buffers
;; Switching buffers is the most essential operation in Emacs.
;; Most of these commands are bound on C-x.
(general-def
  "C-x b"
  (defun akirak/switch-to-project-file-buffer (project)
    (interactive (list (-some-> (project-current)
                         (project-roots)
                         (car-safe))))
    (let ((default-directory (or project default-directory)))
      (helm :prompt (format "Project %s: " project)
            :sources
            `(,@(akirak/helm-project-buffer-sources project #'akirak/switch-to-project-file-buffer)
              ,akirak/helm-source-recent-files
              ,(helm-make-source "Git repositories" 'akirak/helm-source-magit-repos
                 :action '(("Switch to project" . akirak/switch-to-project-file-buffer)
                           ("Magit status" . magit-status)))))))
  "C-x p"
  (defun akirak/find-file-recursively (root)
    (interactive (list (if current-prefix-arg
                           (read-directory-name "Find files in dir: ")
                         (akirak/project-root default-directory))))
    (require 'my/helm/source/file)
    (let ((default-directory root))
      (helm :prompt (format "Browse %s: " root)
            :sources akirak/helm-source-project-files)))
  "C-x d"
  (defun akirak/switch-to-dired-buffer ()
    (interactive)
    (pcase current-prefix-arg
      ('(16) (helm :prompt "Git repositories: "
                   :sources akirak/helm-magic-list-repos-source))
      ('(4)
       (if-let (root (akirak/project-root default-directory))
           (let ((default-directory root))
             (helm :prompt "Project: "
                   :sources akirak/helm-project-root-and-ancestors-source))
         (error "Not implemented for outside of a project")))
      ('()
       (helm :prompt "Switch to a dired buffer: "
             :sources
             (list (akirak/helm-dired-buffer-source)
                   akirak/helm-open-buffer-directories-source
                   akirak/helm-directory-bookmark-source)))))
  "C-x j"
  (defun akirak/switch-to-org-buffer ()
    (interactive)
    (require 'helm-org-ql)
    (require 'org-recent-headings)
    (helm :prompt "Switch to Org: "
          :sources
          (list (akirak/helm-indirect-org-buffer-source)
                helm-source-org-recent-headings
                akirak/helm-source-org-starter-known-files
                helm-source-org-ql-views)))
  "C-x '"
  (defun akirak/switch-to-reference-buffer ()
    (interactive)
    (helm :prompt "Switch to a reference buffer: "
          :sources (akirak/helm-reference-buffer-source))))

;; In the list of project buffers, you can switch to a file list with
;; ~M-/~.
(general-def
  :keymaps 'akirak/helm-project-buffer-map
  :package 'my/helm/source/complex
  "M-/" (lambda ()
          (interactive)
          (helm-run-after-quit
           (lambda ()
             (akirak/find-file-recursively default-directory)))))

;; I haven't bound any key to this command yet.
(defun akirak/switch-to-scratch-buffer ()
  (interactive)
  (helm :prompt "Switch to a scratch/REPL buffer: "
        :sources
        (akirak/helm-scratch-buffer-source)))

;;;; Editing
;;;;; Undo and redo
;; You still can use the built-in undo command with C-x u
(use-package undo-fu
  :general
  ("C-/" #'undo-fu-only-undo
   "C-?" #'undo-fu-only-redo))

;;;;; Editing source code comments in org-mode using outorg
;; Bind ~C-c '~ to outorg, which is the same keybinding as =org-edit-special=.
(use-package outorg
  :commands (outorg-edit-as-org)
  :config/el-patch
  (el-patch-defun outorg-convert-oldschool-elisp-buffer-to-outshine ()
    "Transform oldschool elisp buffer to outshine.
In `emacs-lisp-mode', transform an oldschool buffer (only
semicolons as outline-regexp) into an outshine buffer (with
outcommented org-mode headers)."
    (save-excursion
      (goto-char (point-min))
      (when (outline-on-heading-p)
        (outorg-convert-oldschool-elisp-headline-to-outshine))
      (while (not (eobp))
        (outline-next-heading)
        (outorg-convert-oldschool-elisp-headline-to-outshine)))
    (el-patch-remove (funcall 'outshine-hook-function))))
(general-def :keymaps 'emacs-lisp-mode-map
  "C-c '" #'outorg-edit-as-org)
(general-def :keymaps 'outorg-edit-minor-mode-map :package 'outorg
  "C-c '" #'outorg-copy-edits-and-exit)

;;;; Running external commands
(general-def
  "C-x c" #'counsel-compile)

;;;; Maintenance and development of the config
;; These commands are used to maintain this Emacs configuration.
(general-def
  "C-x M-m"
  (defun akirak/helm-my-library ()
    "Browse the library for this configuration."
    (interactive)
    (require 'my/helm/source/file)
    (let ((default-directory (f-join user-emacs-directory "lisp")))
      (helm :prompt (format "Files in %s: " default-directory)
            :sources (list (helm-make-source "Files in project"
                               'akirak/helm-source-project-file)
                           (helm-build-dummy-source "New file in lisp directory"
                             :action #'find-file))))))

;;;; Administration
;;;;; Docker
(akirak/bind-admin
  "k" '(nil :wk "docker")
  "ki" #'docker-images
  "kk" #'docker-containers
  "kn" #'docker-networks
  "kv" #'docker-volumes)
