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

(when (version< emacs-version "27.1")
  (error "Use GNU Emacs version 27.1 or later"))

(let ((local-custom-file "~/local/emacs/custom.el"))
  (when (file-exists-p local-custom-file)
    (setq custom-file local-custom-file)
    (load-file custom-file)))

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

(setq straight-x-pinned-packages
      '(("dracula-theme" . "11391ea531d40fb08c64313bbb86e4d29d7fe1c5")))

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

;;;; Keybindings

(use-package which-key

  :init
  (which-key-mode t)
  :config
  (which-key-setup-side-window-bottom)

  (defmacro akirak/which-key-add-stripped-prefix (prefix)
    "Add PREFIX as a stripped prefix to `which-key-replacement-alist'."
    `(add-to-list 'which-key-replacement-alist
                  (quote ((nil . ,prefix) .
                          (lambda (kb)
                            (cons (car kb)
                                  (string-remove-prefix ,prefix (cdr kb))))))))

  (akirak/which-key-add-stripped-prefix "akirak/")
  (akirak/which-key-add-stripped-prefix "helm-org-multi-wiki-create/"))

;; Use general.el to define keybindings. It has made several
;; improvements over bind-key, including a built-in integration with
;; which-key.

;; This also adds support for =:general= keyword in use-package
;; directives.
(use-package general
  :config

  (general-create-definer akirak/bind-search :prefix "M-s")
  (general-create-definer akirak/bind-jump :prefix "M-g")
  (general-create-definer akirak/bind-register :prefix "C-x r")

  (general-create-definer akirak/bind-help :prefix "<f1>")
  (general-create-definer akirak/bind-file-extra :prefix "<f6>")
  ;; <f7> is currently free
  (general-create-definer akirak/bind-f8 :prefix "<f8>")
  ;; <f9> is reserved for recompile
  (general-create-definer akirak/bind-admin :prefix "<f12>"
    :prefix-map 'akirak/admin-map)

  ;; ~C-c~ is reserved for the user.
  ;; Package developers should not use them for their packages.
  (general-create-definer akirak/bind-user :prefix "C-c")

  ;; bind-generic (C-.) for editing
  ;; Generic prefix key for editing commands.
  (general-create-definer akirak/bind-generic :prefix "C-."
    :prefix-map 'akirak/generic-prefix-map)

  ;; bind-mode (C-,) for major-mode-specific commands
  (defconst akirak/mode-prefix-key "C-,"
    "Prefix for mode-specific keys.")
  (general-create-definer akirak/bind-mode :prefix akirak/mode-prefix-key)

  ;; Use ~<C-return>~ for starting a REPL session
  (general-create-definer akirak/bind-mode-repl
    :prefix "<C-return>")

  ;; TODO: I want to change this key to something else
  (general-create-definer akirak/bind-customization :prefix "C-x ESC"))

(use-package defrepeater
  :general
  ([remap other-window] (defrepeater #'other-window)
   [remap winner-undo] (defrepeater #'winner-undo)
   [remap winner-redo] (defrepeater #'winner-redo)
   [remap text-scale-increase] (defrepeater #'text-scale-increase)
   [remap text-scale-decrease] (defrepeater #'text-scale-decrease)))

;;;; Default settings
(require 'setup-defaults)

(when (akirak/running-on-crostini-p)
  (require 'my/system/platform/crostini))

;;;; Migrating
;; In case there are functions that depends on these modules,
;; load them first.
(require 'my/project)
(require 'my/buffer/predicate)
(org-babel-load-file (expand-file-name "main.org" user-emacs-directory))

;;; Packages
(use-package discover-my-major
  :commands (discover-my-major))
(use-package docker
  :disabled t)
(use-package ediprolog)
(use-package electric
  :straight (:type built-in)
  :hook
  (text-mode . electric-pair-local-mode))
(use-package epkg)
(use-package explain-pause-mode)

(use-package helm-tail
  :commands (helm-tail))
(use-package org-recent-headings
  :after org
  :config
  (org-recent-headings-mode 1))
(use-package su)
(use-package valign
  :disabled t
  :hook
  (org-mode . valign-mode))
(use-package whole-line-or-region)

;;; Commands and keybindings
;;;; Basic keybindings
;; These keybindings basically emulate UNIX shells (i.e. sh, bash,
;; etc.).
;;
;; I also like to define "dwim" commands, if applicable, to save the
;; keybinding space and key strokes.
;;;;; C-a
;; By default, ~C-a~ is bound to =beginning-of-line=.
;;
;; This command first jump to the indentation and then visits the
;; beginning of line.
(general-def prog-mode-map
  "C-a"
  (defun akirak/back-to-indentation-or-beginning-of-line ()
    (interactive)
    (if (or (looking-at "^")
            (string-match-p (rx (not (any space)))
                            (buffer-substring-no-properties
                             (line-beginning-position)
                             (point))))
        (back-to-indentation)
      (beginning-of-line))))

;; In =org-mode=, I prefer =org-beginning-of-line=.
(general-def :keymaps 'org-mode-map :package 'org
  "C-a" #'org-beginning-of-line)

;;;;; C-e
(general-def :keymaps 'org-mode-map :package 'org
  "C-e" #'org-end-of-line)

;;;;; C-h
(general-def
  "C-h" 'backward-delete-char)

;;;;; C-w
(general-def
  "C-w"
  (defun akirak/kill-region-or-backward-kill-word (&optional arg)
    "If a region is active, run `kill-region'. Otherwise, run `backward-kill-word'."
    (interactive "p")
    (if (region-active-p)
        (kill-region (region-beginning) (region-end))
      (backward-kill-word arg))))

(general-def minibuffer-local-map
  "C-w" #'backward-kill-word)

(general-def ivy-minibuffer-map :package 'ivy
  "C-w" #'ivy-backward-kill-word)

;;;;; C-u
(general-def minibuffer-local-map
  "C-u" #'backward-kill-sentence)

(general-def ivy-minibuffer-map :package 'ivy
  "C-u"
  (defun ivy-backward-kill-sentence ()
    (interactive)
    (if ivy--directory
        (progn (ivy--cd "/")
               (ivy--exhibit))
      (if (bolp)
          (kill-region (point-min) (point))
        (backward-kill-sentence)))))

;;;;; C-r
;; In minibuffers, ~C-r~ should call history.
(general-def ivy-minibuffer-map :package 'ivy
  "C-r" 'counsel-minibuffer-history)

;;;; Key translation and simulation
;; Since I have bound C-h to =backward-delete-char= but still use the
;; help system frequently, I bind ~M-`~ to ~<f1>~ in
;; =key-translation-map=.
(general-def key-translation-map
  ;; * Obsolete
  ;; As <menu> (application on Windows keyboards) is hard to reach on some
  ;; keyboards, I will use <C-tab> instead. This key combination is occupied on
  ;; web browsers but vacant on most Emacs major modes, so it is safe to use it
  ;; on non-EXWM buffers.
  ;; "<C-tab>" (kbd "<menu>")

  ;; Chromebook don't have physical function keys. They substitute
  ;; Search + num for function keys, but Search + 1 is hard to press,
  ;; especially when Search and Ctrl are swapped.
  ;; This is quite annoying, so I will use M-` as <f1>.
  "M-`" (kbd "<f1>"))

(general-def "M-r" (general-simulate-key "C-x r"))

;;;;; Emulate virtual function keys of Chrome OS
;; Emulate function keys of Chrome OS, i.e. use ~s-NUM~ as function
;; keys.
(define-globalized-minor-mode akirak/emulate-chromeos-fnkey-mode
  nil
  (lambda ()
    (cond
     (akirak/emulate-chromeos-fnkey-mode
      (dolist (n (number-sequence 1 9))
        (define-key key-translation-map
          (kbd (format "s-%d" n)) (kbd (format "<f%d>" n))))
      (define-key key-translation-map
        (kbd "s-0") (kbd "<f10>"))
      (define-key key-translation-map
        (kbd "s--") (kbd "<f11>"))
      (define-key key-translation-map
        (kbd "s-=") (kbd "<f12>")))
     (t
      (dolist (n (number-sequence 0 9))
        (define-key key-translation-map
          (kbd (format "s-%d" n)) nil))
      (define-key key-translation-map
        (kbd "s--") nil)
      (define-key key-translation-map
        (kbd "s-=") nil)))))

(unless (akirak/running-on-crostini-p)
  (akirak/emulate-chromeos-fnkey-mode 1))

;;;; Switching buffers
;; Switching buffers is the most essential operation in Emacs.
;; Most of these commands are bound on C-x.
;;;;; Helm commands
(general-def
  "C-x b"
  (defun akirak/switch-to-project-file-buffer (project)
    (interactive (list (if current-prefix-arg
                           'all
                         (-some-> (project-current)
                           (project-roots)
                           (car-safe)))))
    (require 'my/helm/action/git)
    (cond
     ((eq project 'all)
      (helm-buffers-list))
     (t
      (let ((default-directory (or project default-directory)))
        (helm :prompt (format "Project %s: " project)
              :sources
              `(,@(akirak/helm-project-buffer-sources project #'akirak/switch-to-project-file-buffer)
                ,akirak/helm-source-recent-files))))))
  "C-x p"
  (defun akirak/find-file-recursively (root)
    (interactive (list (akirak/project-root default-directory)))
    (require 'my/helm/source/file)
    (when current-prefix-arg
      (akirak/clear-project-file-cache root :sort 'modified))
    (let ((default-directory root))
      (helm :prompt (format "Browse %s: " root)
            :sources
            (list akirak/helm-source-project-files
                  akirak/helm-source-dummy-find-file))))
  "C-x d"
  (defun akirak/switch-to-dired-buffer ()
    "Switch to a directory buffer interactively.

Without a prefix, it displays a list of dired buffers, a list of
directories of live file buffers, and a list of directory
bookmarks.

With a single universal prefix, it displays a list of known Git
repositories.

With two universal prefixes, it displays a list of remote
connection identities of recent files."
    (interactive)
    (pcase current-prefix-arg
      ('(4)
       (require 'my/helm/source/remote)
       (helm :prompt "Remote: "
             :sources
             '(akirak/helm-source-remote-bookmark
               akirak/helm-source-recent-remotes)))
      ('()
       (require 'my/helm/source/dir)
       (helm :prompt "Directory/repository: "
             :sources
             (list (akirak/helm-dired-buffer-source)
                   akirak/helm-directory-bookmark-source
                   akirak/helm-open-file-buffer-directories-source)))
      (_ (user-error "Not matching %s" current-prefix-arg))))
  "C-x g"
  (defun akirak/browse-git-repository ()
    (interactive)
    (require 'my/helm/source/dir)
    (helm :prompt "Directory/repository: "
          :sources
          (list akirak/helm-directory-bookmark-as-git-source
                akirak/helm-magit-list-repos-source)))
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
  (defun akirak/switch-to-reference-buffer-or-browser ()
    (interactive)
    (require 'my/helm/source/web)
    (helm :prompt "Switch to a reference buffer: "
          :default (list (thing-at-point 'symbol)
                         (buffer-name helm-current-buffer))
          :sources (append (list (akirak/helm-reference-buffer-source))
                           (list helm-source-bookmark-info
                                 helm-source-bookmark-man)
                           (list (helm-def-source--info-files))
                           (akirak/helm-web-sources)))))

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
;;;;; Avy
;; Use avy-goto-word-1.
;; https://irreal.org/blog/?p=9130
(general-def
  "C-'" #'avy-goto-word-1)

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

(general-def :keymaps 'Info-mode-map :package 'info
  "h" #'Info-up
  [remap forward-page] #'Info-next-preorder
  [remap backward-page] #'Info-prev)
;;;; Help and documentation
;;;;; Additional keybindings on <f1>
(akirak/bind-help
  "M" #'discover-my-major
  "xc" #'describe-char
  "xf" #'counsel-faces)

;; e.g. M-` M-m -> <f1> ESC m
(akirak/bind-help
  "ESC m" #'woman
  "ESC i" #'helm-info
  "ESC d" #'helm-dash)

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

;;;;; Formatting code
(akirak/bind-generic
  "lf"
  (defun akirak/run-formatter ()
    (interactive)
    (require 'my/formatter)
    (pcase (akirak/get-project-formatter)
      (`(reformatter ,name)
       (if (region-active-p)
           (funcall (intern (concat name "-region")))
         (funcall (intern (concat name "-buffer"))))
       (let ((error-buf (get-buffer (format "*%s errors*" name))))
         (if (and error-buf
                  (> (buffer-size error-buf) 0))
             (display-buffer error-buf)
           (when-let (w (and error-buf
                             (get-buffer-window error-buf)))
             (quit-window nil w)))))
      (_ (user-error "%s formatter" formatter)))))

(akirak/bind-mode :keymaps 'magit-status-mode-map :package 'magit-status
  "lf"
  (defun akirak/run-formatter-on-project ()
    (interactive)
    (require 'my/formatter)
    (require 'my/file/enum)
    (let* ((project default-directory)
           (files (akirak/project-files project))
           (alist (->> (-group-by #'f-ext files)
                       (-sort (lambda (a b)
                                (> (length (cdr a))
                                   (length (cdr b)))))
                       (-filter #'car)))
           (ext (completing-read "File extension: "
                                 (-map #'car alist)
                                 nil t)))
      (dolist (file (cdr (assoc ext alist)))
        (let (new-buffer)
          (with-current-buffer (or (find-buffer-visiting file)
                                   (setq new-buffer
                                         (find-file-noselect file)))
            (save-restriction
              (widen)
              (pcase (akirak/get-project-formatter project :mode major-mode)
                (`(reformatter ,name)
                 (funcall (intern (concat name "-buffer"))))
                (_ (user-error "%s formatter" formatter)))
              (save-buffer))
            (let ((error-buf (get-buffer (format "*%s errors*" name))))
              (if (and error-buf
                       (> (buffer-size error-buf) 0))
                  (progn
                    (switch-to-buffer (current-buffer))
                    (display-buffer error-buf)
                    (user-error "Error while applying the formatter"))
                (when-let (w (and error-buf
                                  (get-buffer-window error-buf)))
                  (quit-window nil w)))))
          (when new-buffer
            (kill-buffer new-buffer)))))
    (if (derived-mode-p 'magit-status-mode)
        (progn
          (message "Finished formatting. Refreshing the magit buffer...")
          (magit-refresh))
      (message "Finished formatting"))))

;;;; Running external commands
(general-def
  "C-x c"
  (defun akirak/compile-command (&optional arg)
    (interactive "P")
    (require 'my/compile)
    (cl-labels
        ((spago-root
          ()
          (locate-dominating-file default-directory "spago.dhall"))
         (spago-build
          (root)
          (let ((command (completing-read "PureScript spago command: "
                                          akirak/spago-compile-command-list)))
            (akirak/compile command :directory root)))
         (make-root
          ()
          (locate-dominating-file default-directory "Makefile"))
         (npm-root
          ()
          (locate-dominating-file default-directory "package.json"))
         (npm-run-something
          (root)
          (progn
            (require 'my/compile/npm)
            (let ((script-alist (akirak/npm-package-json-commands (f-join root "package.json")))
                  (default-directory root)
                  (action (lambda (command)
                            (akirak/compile (concat "npm " command)
                                            :nix-shell-args (unless (executable-find "npm")
                                                              '("-p" "nodejs"))))))
              (helm :prompt (format "npm command [%s]: " root)
                    :sources
                    (list (helm-build-sync-source "Script"
                            :candidates
                            (-map (lambda (cell)
                                    (cons (format "%s: %s" (car cell) (cdr cell))
                                          (cdr cell)))
                                  script-alist)
                            :coerce (-partial #'s-append "run ")
                            :action action)
                          (helm-build-sync-source "Basic commands"
                            :candidates
                            '("install")
                            :action action))))))
         (mix-run-command
          ()
          (progn
            (require 'my/compile/mix)
            (helm :prompt (format "mix command [%s]: " default-directory)
                  :sources
                  (list (helm-build-sync-source "Mix commands"
                          :candidates
                          (-map (lambda (cell)
                                  (cons (format "%s %s"
                                                (car cell)
                                                (propertize (cdr cell)
                                                            'face 'font-lock-comment-face))
                                        (car cell)))
                                (akirak/mix-command-alist))
                          :action
                          `(("compile" . akirak/compile)
                            ("compile (with args)" .
                             (lambda (command)
                               (akirak/compile (read-string "Command: " command)))))))))))
      (let (root)
        (cond
         ((and (derived-mode-p 'purescript-mode)
               (setq root (spago-root)))
          (spago-build root))
         ((equal arg '(4))
          (helm :prompt "Compile history: "
                :sources akirak/helm-compile-history-source))
         ((equal arg 0)
          (let ((root (akirak/project-root default-directory)))
            (if (and root (f-exists (f-join root ".github"))
                     (executable-find "act"))
                (let ((default-directory root))
                  (compile "act"))
              (user-error "N/A"))))
         ((and (setq root (akirak/project-root default-directory))
               (f-exists-p (f-join root "package.json")))
          (npm-run-something root))
         ((and root
               (f-exists-p (f-join root "Makefile")))
          (let ((default-directory root)) (counsel-compile)))
         ((and root
               (f-exists-p (f-join root "mix.exs")))
          (let ((default-directory root))
            (mix-run-command)))
         (t
          (akirak/helm-shell-command root))))))
  "C-x C"
  (defun akirak/helm-shell-command (&optional root)
    (interactive)
    (require 'my/helm/source/org)
    (require 'my/helm/action/org-marker)
    (let ((root (or root
                    (akirak/project-root default-directory)
                    default-directory)))
      (setq akirak/programming-recipe-mode-name "sh"
            akirak/helm-org-ql-buffers-files (org-multi-wiki-entry-files 'default :as-buffers t))
      (helm :prompt (format "Execute command (project root: %s): " root)
            :sources
            (list (helm-make-source "Command" 'akirak/helm-source-org-ql-src-block
                    :action akirak/helm-org-marker-sh-block-action-list)
                  (helm-build-dummy-source "Command"
                    :action
                    `(("compile"
                       . (lambda (command)
                           (akirak/compile command :directory ,root)))
                      ("eshell"
                       . (lambda (command)
                           (let ((default-directory ,root))
                             (eshell-command command)))))))))))

;;;; Maintenance and development of the config
;; These commands are used to maintain this Emacs configuration.
(akirak/bind-customization
  "" '(nil :wk "customize")
  "f" #'customize-face-other-window
  "o" #'customize-group-other-window
  "l" #'counsel-find-library
  "p" '((lambda () (interactive)
          (if (featurep 'straight)
              (call-interactively 'straight-use-package)
            (package-list-packages)))
        :wk "packages")
  "s" #'customize-set-value
  "v" #'customize-variable-other-window)

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

;;;; Per-project
(cl-defmacro akirak/run-at-project-root (command &key other-window)
  `(defun ,(intern (concat "akirak/project-" (symbol-name command))) ()
     (interactive)
     (let ((default-directory (akirak/project-root default-directory)))
       (when ,other-window
         (or (other-window 1)
             (split-window-sensibly)))
       (call-interactively (quote ,command)))))

(akirak/bind-f8
  "c" (akirak/run-at-project-root compile)
  "d" (defun akirak/project-dired ()
        (interactive)
        (dired (akirak/project-root default-directory)))
  "D" (akirak/run-at-project-root add-dir-local-variable)
  "e" (akirak/run-at-project-root ielm :other-window t)
  "g" #'deadgrep
  "n" (akirak/run-at-project-root nix-repl :other-window t)
  "t" (akirak/run-at-project-root vterm :other-window t))

;;;; Administration
;;;;; Directory/disk
(akirak/bind-admin
  "d" '(nil :wk "dir")
  "de" #'direnv-allow
  "du" #'disk-usage
  "dh" #'helm-linux-disks)

;;;;; Emacs
(akirak/bind-admin
  "ex" #'explain-pause-mode
  "et" #'ert)

;;;;; Git
(akirak/bind-admin
  "g" '(nil :wk "git")
  "gb" #'commonplace-find-repo-module
  "gc" #'akirak/git-clone-remote-repo
  "gl" #'magit-list-repositories
  "go" #'akirak/github-owned-repos
  "gr" #'commonplace-repos-counsel-rg
  "gs" #'akirak/github-starred-repos
  "gu" #'akirak/github-users)

;;;;; Docker
(akirak/bind-admin
  "k" '(nil :wk "docker")
  "ki" #'docker-images
  "kk" #'docker-containers
  "kn" #'docker-networks
  "kv" #'docker-volumes)

;;;;; Nix
(akirak/bind-admin
  "n" '(nil :wk "nix")
  "ni" #'nix-boilerplate-init
  "nn" #'nix-env-install-npm)

;;;;; Remote connections (TRAMP)
(akirak/bind-admin
  "r" '(nil :wk "remote")
  "rk" #'helm-delete-tramp-connection)
