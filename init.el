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

(when (version< emacs-version "26.1")
  (error "Use GNU Emacs version 26.1 or later"))

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
(akirak/straight-use-recipes-from-file
 akirak/straight-default-recipes-file)

;;;; Load configuration files

(add-to-list 'load-path (expand-file-name "lisp" user-emacs-directory))
(require 'my/const/system)

;; TODO: Move to lisp/
(add-to-list 'load-path (expand-file-name "extras" user-emacs-directory))

;;; Configuration
;; Prevent a confirmation dialog when the org file is loaded.
;; Don't forget to revert this variable at the beginning of the Org file.
(setq-default enable-local-variables :all)
(load-file (expand-file-name "core/setup.el" user-emacs-directory))

;;;; Things to set up before using =use-package=
(akirak/require 'setup-gc)

(setq-default enable-local-variables :safe)

;; These packages are required in other use-package directives declared in this
;; configuration.

(use-package el-patch
  :custom
  (el-patch-enable-use-package-integration t))

;; Activate =package.el= for loading built-in packages from nixpkgs:

(require 'package)
(package-initialize 'noactivate)

;; Package-specific configuration files, including snippets, are kept in [[https://github.com/akirak/emacs-config-library][a separate repository]], not in this repository.

(use-package no-littering)

;; Use the executable path from the shell

(use-package exec-path-from-shell
  :disabled t
  :if (memq window-system '(mac ns x))
  :init
  (exec-path-from-shell-initialize))

;; Use diminish to reduce clutters from the modeline. This adds support for =:diminish= keyword:

(use-package diminish
  :disabled t
  :init
  (diminish 'auto-revert-mode)
  (diminish 'outline-minor-mode)
  (diminish 'flyspell-mode))

(use-package use-package-company
  ;; Originally written by Foltik, but I use my fork
  :straight (use-package-company :host github :repo "akirak/use-package-company"))

(use-package general)

;;;; Default settings
(require 'setup-defaults)

(when (akirak/running-on-crostini-p)
  (require 'my/system/platform/crostini))

;;;; Migrating
(org-babel-load-file (expand-file-name "main.org" user-emacs-directory))

;;; Packages
(use-package docker)
(use-package helm-tail
  :commands (helm-tail))
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
;;;;; Helm commands
(general-def
  "C-x b"
  (defun akirak/switch-to-project-file-buffer (project)
    (interactive (list (-some-> (project-current)
                         (project-roots)
                         (car-safe))))
    (require 'my/helm/action/git)
    (let ((default-directory (or project default-directory)))
      (helm :prompt (format "Project %s: " project)
            :sources
            `(,@(akirak/helm-project-buffer-sources project #'akirak/switch-to-project-file-buffer)
              ,akirak/helm-source-recent-files
              ,(helm-make-source "Git repositories" 'akirak/helm-source-magit-repos
                 :action (cons '("Switch to project" . akirak/switch-to-project-file-buffer)
                               akirak/helm-git-project-actions))))))
  "C-x p"
  (defun akirak/find-file-recursively (root)
    (interactive (list (if current-prefix-arg
                           (read-directory-name "Find files in dir: ")
                         (akirak/project-root default-directory))))
    (require 'my/helm/source/file)
    (let ((default-directory root))
      (helm :prompt (format "Browse %s: " root)
            :sources
            (list akirak/helm-source-project-files
                  akirak/helm-source-dummy-find-file))))
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
  "C-x x"
  (defun akirak/switch-to-x-buffer (&optional arg)
    (interactive "P")
    (cond
     ((akirak/exwm-session-p)
      (helm :prompt "Switch to EXWM buffer: "
            :sources (akirak/helm-exwm-buffer-source)))
     ((akirak/windows-subsystem-for-linux-p)
      (user-error "Not supported on WSL"))
     ((eq system-type 'linux)
      ;; TODO: Implement it
      (cl-assert (executable-find "wmctrl"))
      (helm :prompt "X window: "
            :source
            (helm-build-sync-source "X windows"
              :candidates (-map (lambda (s) (cons s (car (s-split-words s))))
                                (process-lines "wmctrl" "-l"))
              :action (lambda (wid)
                        (async-start-process "wmctrl" "wmctrl" nil
                                             "-a" wid)))))))
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

;;;;; Browsing contents in specific buffers without leaving the context
(general-def
  ;; This command lets you browse lines in error buffers.
  "C-x t" #'helm-tail)
;;;; Navigation in buffer
;;;;; Page navigation
;; I will use ~C-x [~ and ~C-x ]~ for "page" navigation. These keys
;; are bound to =backward-page= and =forward-page= by default, but
;; they should be rebound depending on the major mode, since the
;; notion of page/chunk varies.

(general-def
  ;; Default
  "C-x [" #'backward-page
  "C-x ]" #'forward-page)

(general-def :keymaps 'org-mode-map :package 'org
  ;; [remap backward-page]
  [remap forward-page]
  (defun akirak/org-narrow-to-next-sibling-subtree ()
    (interactive)
    (if (buffer-narrowed-p)
        (let ((old-level (save-excursion
                           (goto-char (point-min))
                           (org-outline-level)))
              (end (point-max)))
          (goto-char (point-max))
          (widen)
          (if (re-search-forward org-heading-regexp nil t)
              (let ((new-level (org-outline-level)))
                (org-narrow-to-subtree)
                (org-back-to-heading)
                (org-show-subtree)
                (cond
                 ((= new-level old-level)
                  (message "Narrowing to the next sibling"))
                 ((> new-level old-level)
                  (message "Narrowing to a child"))
                 ((< new-level old-level)
                  (message "Narrowing to an upper level"))))
            (message "No more heading")))
      (message "Buffer is not narrowed"))))

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
  "C-x c"
  (defun akirak/compile (&optional arg)
    (interactive "P")
    (pcase arg
      ('(4)
       (akirak/helm-shell-command))
      (0
       (let ((root (akirak/project-root default-directory)))
         (if (and root (f-exists (f-join root ".github"))
                  (executable-find "act"))
             (let ((default-directory root))
               (compile "act"))
           (user-error "N/A"))))
      (_
       (counsel-compile)))))

(defun akirak/helm-shell-command ()
  (interactive)
  (require 'my/helm/source/org)
  (require 'my/helm/action/org-marker)
  (let ((root (or (akirak/project-root default-directory)
                  default-directory)))
    (setq akirak/programming-recipe-mode-name "sh"
          akirak/helm-org-ql-buffers-files (org-multi-wiki-entry-files 'organiser :as-buffers t))
    (helm :prompt (format "Execute command (project root: %s): " root)
          :sources
          (helm-make-source "Command" 'akirak/helm-source-org-ql-src-block
            :action
            `(("compile at project root"
               . (lambda (marker)
                   (akirak/helm-execute-sh-src-block-action marker #'compile :project t)))
              ("compile at working directory"
               . (lambda (marker)
                   (akirak/helm-execute-sh-src-block-action marker #'compile)))
              ("eshell command at project root"
               . (lambda (marker)
                   (akirak/helm-execute-sh-src-block-action marker #'eshell-command :project t)))
              ("eshell command at working directory"
               . (lambda (marker)
                   (akirak/helm-execute-sh-src-block-action marker #'eshell-command)))
              ("Show the whole entry" . helm-org-ql-show-marker-indirect))))))

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
