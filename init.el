;;; About this configuration
;; This is my Emacs configuration.
;;;; Prerequisites
;; - Linux or Window Subsystem for Linux
;; - Nix package manager for installing dependencies

;;;; Installation
;; This configuration depends on some external programs as well as Emacs
;; Lisp packages built by Nix.

;; At present, a suggested installation procedure is to first install my [[https://github.com/akirak/home.nix][home.nix]] and then run =mr checkout= at your home directory.
;; In the future, it may support an alternative for trying out.

;;;; LICENSE
;; GPL v3.

;; Some libraries were originally written by other people, and they follow their respective licenses.

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

;;;; Load configuration files
(load-file (expand-file-name "core/setup.el" user-emacs-directory))

(load-file (expand-file-name "straight-recipes.el" user-emacs-directory))

(add-to-list 'load-path (expand-file-name "lisp" user-emacs-directory))
;; TODO: Move to lisp/
(add-to-list 'load-path (expand-file-name "extras" user-emacs-directory))

;; Prevent a confirmation dialog when the org file is loaded.
;; Don't forget to revert this variable at the beginning of the Org file.
(setq-default enable-local-variables :all)

(org-babel-load-file (expand-file-name "main.org" user-emacs-directory))

;;; Commands
;;;; Switching buffers
;; Most of these commands are bound on C-x

(use-package org-recent-headings
  :after org
  :config
  (org-recent-headings-mode 1))

(use-package my/project
  :straight (:type built-in))

(use-package my/buffer/predicate
  :straight (:type built-in))

(use-package my/dir/enum
  :straight (:type built-in))

(use-package my/helm/source/buffer
  :straight (:type built-in))

(use-package my/helm/source/file
  :straight (:type built-in))

(use-package my/helm/source/dir
  :straight (:type built-in))

(defvar akirak/directory-contents-cache nil)

(defvar akirak/helm-project-buffer-map
  (let ((map (copy-keymap helm-map)))
    (define-key map (kbd "M-/")
      (lambda ()
        (interactive)
        (helm-run-after-quit (lambda () (akirak/find-file-recursively project)))))
    map))

(cl-defun akirak/switch-to-project-file-buffer (project)
  (interactive (list (-some-> (project-current)
                       (project-roots)
                       (car-safe))))
  (require 'my/helm/action/buffer)
  (require 'my/helm/action/file)
  (setq akirak/switch-buffer-project project)
  (cl-labels ((root-of (buffer)
                       (akirak/project-root (buffer-dir buffer)))
              (buffer-dir (buffer)
                          (buffer-local-value 'default-directory buffer))
              (format-mode (buffer)
                           (format "[%s]" (buffer-local-value 'major-mode buffer)))
              (format-fbuf (buffer)
                           (let ((root (root-of buffer))
                                 (file (buffer-file-name buffer))
                                 (modified (buffer-modified-p buffer)))
                             (concat (if modified "* " "")
                                     (if root
                                         (format "%s > %s "
                                                 (f-short root)
                                                 (and root (f-relative file root)))
                                       (f-short file))
                                     " "
                                     (format-mode buffer))))
              (same-project-p (buf)
                              (-some->> (root-of buf)
                                (file-equal-p project)))
              (project-bufp (buf)
                            (not (f-ancestor-of-p "~/lib/" (buffer-file-name buf))))
              (file-buffer-cell (buffer)
                                (cons (format-fbuf buffer) buffer))
              (kill-project-bufs (project)
                                 (let ((bufs (-filter (lambda (buf)
                                                        (let ((dir (buffer-dir buf)))
                                                          (or (f-equal-p dir project)
                                                              (f-ancestor-of-p project dir))))
                                                      (buffer-list))))
                                   (when (yes-or-no-p (format "Kill all buffers in %s" project))
                                     (mapc #'kill-buffer bufs)
                                     (helm-run-after-quit (lambda () (akirak/switch-to-project-file-buffer project)))))))
    (-let* ((file-buffers (-filter #'buffer-file-name (buffer-list)))
            ((same-project-buffers other-file-buffers)
             (if project (-separate #'same-project-p file-buffers) (list nil file-buffers)))
            (same-project-other-buffers
             (-remove-item (current-buffer) same-project-buffers))
            (other-project-buffers (-filter #'project-bufp other-file-buffers))
            (other-projects (->> (-map #'root-of other-project-buffers)
                                 (delq nil)
                                 (-uniq))))
      (helm :prompt (format "Project %s: " project)
            :sources
            (list (cond
                   (same-project-buffers
                    (helm-build-sync-source (format "File buffers in project %s"
                                                    project)
                      :candidates (mapcar #'file-buffer-cell
                                          (or same-project-other-buffers
                                              same-project-buffers))
                      :keymap akirak/helm-project-buffer-map
                      :action akirak/helm-buffer-actions-1))
                   (project (akirak/helm-project-file-source project)))
                  (helm-build-sync-source "File buffers in other projects"
                    :candidates (mapcar #'file-buffer-cell other-project-buffers)
                    :action akirak/helm-buffer-actions-1)
                  (helm-build-sync-source "Other projects with open file buffers"
                    :candidates other-projects
                    :persistent-action #'kill-project-bufs
                    :action '(("Switch to project" . akirak/switch-to-project-file-buffer)
                              ("Magit status" . magit-status)))
                  (helm-build-sync-source "Recentf"
                    :candidates (-map #'f-short recentf-list)
                    :action akirak/helm-file-actions)
                  (helm-build-sync-source "Git repositories"
                    :candidates (->> (magit-repos-alist)
                                     (-map #'cdr)
                                     (-map #'f-short))
                    :action '(("Switch to project" . akirak/switch-to-project-file-buffer)
                              ("Magit status" . magit-status))))))))

(defvar akirak/switch-buffer-project nil
  "The root directory of the project of interest.")

(general-def
  "C-x b" #'akirak/switch-to-project-file-buffer
  "C-x p"
  (defun akirak/find-file-recursively (root)
    (interactive (list (if current-prefix-arg
                           (read-directory-name "Find files in dir: ")
                         (akirak/project-root default-directory))))
    (setq akirak/switch-buffer-project root)
    (helm :prompt (format "Browse %s: " root)
          :sources (list (akirak/helm-project-file-source root))))
  "C-x d"
  (defun akirak/switch-to-dired-buffer ()
    (interactive)
    (require 'my/helm/source/buffer)
    (require 'my/helm/source/dir)
    (require 'my/helm/source/bookmark)
    (pcase current-prefix-arg
      ('(16) (helm :prompt "Git repositories: "
                   :sources akirak/helm-magic-list-repos-source))
      ('(4)
       (if-let (root (akirak/project-root default-directory))
           (helm :prompt "Project: "
                 :sources
                 (akirak/helm-project-root-and-ancestors-source root))
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
    (require 'my/helm/source/buffer)
    (helm :prompt "Switch to Org: "
          :sources
          (list (akirak/helm-indirect-org-buffer-source)
                helm-source-org-recent-headings
                akirak/helm-source-org-starter-known-files
                helm-source-org-ql-views)))
  "C-x '"
  (defun akirak/switch-to-reference-buffer ()
    (interactive)
    (require 'my/helm/source/buffer)
    (helm :prompt "Switch to a reference buffer: "
          :sources (akirak/helm-reference-buffer-source))))

(defun akirak/switch-to-scratch-buffer ()
  (interactive)
  (require 'my/helm/source/buffer)
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
(general-def "C-c '" #'outorg-edit-as-org)
(general-def :keymaps 'outorg-edit-minor-mode-map :package 'outorg
  "C-c '" #'outorg-copy-edits-and-exit)
