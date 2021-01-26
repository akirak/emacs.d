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
  (if (file-exists-p local-custom-file)
      (setq custom-file local-custom-file)
    (message "%s does not exist, so custom-file is not set"
             local-custom-file)))

(when custom-file
  (load custom-file nil :nomessage))

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
(use-package org
  :straight (:type built-in)
  :config
  (require 'org-loaddefs))

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

(use-package info
  :straight (:type built-in)
  :config
  (add-to-list 'Info-directory-list
               (expand-file-name "share/info"
                                 (file-name-directory
                                  (string-remove-suffix "/" invocation-directory)))))

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

(require 'setup-gpg)

;;;; Migrating
;; In case there are functions that depends on these modules,
;; load them first.
(require 'my/project)
(require 'my/buffer/predicate)
(org-babel-load-file (expand-file-name "main.org" user-emacs-directory))

;;; Packages
(use-package dash-docs)
(use-package helm-dash
  :custom
  (dash-docs-browser-func #'akirak/browse-url))
(use-package discover-my-major
  :commands (discover-my-major))
(use-package electric
  :straight (:type built-in)
  :hook
  (text-mode . electric-pair-local-mode))
(use-package epkg)

(use-package helm-tail
  :commands (helm-tail))
(use-package org-recent-headings
  :after org
  :config
  (general-add-hook 'org-recent-headings-advise-functions
                    '(org-multi-wiki-follow-link
                      org-multi-wiki-visit-entry
                      akirak/avy-org-heading
                      org-insert-heading
                      helm-org-ql-show-marker
                      helm-org-ql-show-marker-indirect))
  (org-recent-headings-mode 1)

  (setq org-recent-headings-reject-any-fns
        (list (defun akirak/org-recent-headings-reject-journal-date (entry)
                (when (featurep 'org-multi-wiki)
                  (let ((file (org-recent-headings-entry-file entry))
                        (olp (org-recent-headings-entry-outline-path entry)))
                    (when-let (plist (org-multi-wiki-entry-file-p file))
                      (and (eq 'journal (plist-get plist :namespace))
                           (= 1 (length olp)))))))))

  (defun akirak/org-recent-headings-cleanup ()
    (let ((m (length org-recent-headings-list))
          (start-time (float-time))
          (n (progn
               (dolist (x org-recent-headings-list)
                 (condition-case _
                     (org-recent-headings--entry-marker x)
                   (error (cl-delete x org-recent-headings-list
                                     :test #'org-recent-headings--equal))))
               (length org-recent-headings-list))))
      (unless (= m n)
        (message "Deleted %d non-existent items from org-recent-headings-list in %.1f s"
                 (- m n)
                 (- (float-time) start-time)))))
  (run-with-idle-timer 50 nil #'akirak/org-recent-headings-cleanup))
(use-package helm-org-recent-headings
  :after (helm org-recent-headings)
  :config
  ;; Modified from `helm-org-recent-headings-source'.
  (defvar akirak/helm-org-recent-headings-source
    (helm-build-sync-source " Recent Org headings"
      :candidates (lambda ()
                    org-recent-headings-list)
      :candidate-number-limit 'org-recent-headings-candidate-number-limit
      :candidate-transformer 'helm-org-recent-headings--truncate-candidates
      :keymap helm-org-recent-headings-map
      :action 'akirak/helm-org-recent-headings-actions)
    "Helm source for `org-recent-headings'.")
  (defvar akirak/helm-org-recent-headings-actions
    (helm-make-actions
     "Show entry (default function)" 'org-recent-headings--show-entry-default
     "Show entry in real buffer" 'org-recent-headings--show-entry-direct
     "Show entry in indirect buffer" 'org-recent-headings--show-entry-indirect
     "Insert a link to the heading"
     (defun akirak/org-recent-headings-insert-link (entry)
       (unless (derived-mode-p 'org-mode)
         (user-error "Not in org-mode"))
       (let ((marker (org-recent-headings--entry-marker entry)))
         (with-current-buffer (marker-buffer marker)
           (org-with-wide-buffer
            (goto-char marker)
            (org-store-link nil 'interactive))))
       (org-insert-last-stored-link 1))
     "Remove entry" 'helm-org-recent-headings-remove-entries
     "Bookmark heading" 'org-recent-headings--bookmark-entry)))
(use-package license-templates)
(use-package su)
(use-package valign
  :disabled t
  :hook
  (org-mode . valign-mode))
(use-package whole-line-or-region)

;;;; Modules
(require 'setup-git-bookmark)
(require 'setup-info)
(require 'setup-unicode)
(require 'setup-mmm)

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

;;;; Insert strings/characters
(defmacro akirak/def-insert-date-time-command (name format)
  `(defun ,(intern (format "akirak/insert-%s" name)) ()
     (interactive)
     (insert (format-time-string ,format))))

;; This prefix map will be overridden in org-mode
(general-def :prefix "C-c !"
  "8" (akirak/def-insert-date-time-command "yyyymmdd-date" "%Y%m%d")
  "f" (akirak/def-insert-date-time-command "iso8601-date" "%F")
  "t" (akirak/def-insert-date-time-command "iso8601-datetime" "%FT%X"))

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
    (interactive (list (or (akirak/project-root default-directory)
                           (akirak/try-init-project-root)
                           (user-error "Cannot find the project root"))))
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
                akirak/helm-magit-list-repos-source
                akirak/helm-toplevel-repos-submodules-source
                akirak/helm-remote-repo-dummy-source
                akirak/helm-github-following-source)))
  "C-x j"
  (defun akirak/switch-to-org-buffer ()
    (interactive)
    (require 'helm-org-ql)
    (require 'org-recent-headings)
    (require 'helm-org-recent-headings)
    (helm :prompt "Switch to Org: "
          :sources
          (list (akirak/helm-indirect-org-buffer-source)
                'akirak/helm-org-clock-context-source
                'akirak/helm-org-recent-headings-source
                ;; akirak/helm-source-org-starter-known-files
                ;; helm-org-ql-views-source
                'akirak/org-agenda-buffer-source
                'akirak/helm-org-ql-dummy-source)))
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
                           (akirak/helm-web-sources))))

  "<f6> <f6>"
  (defun akirak/switch-to-recent-file-buffer ()
    (interactive)
    (if-let (buf (->> (buffer-list)
                      (-filter (lambda (buf)
                                 (and (buffer-file-name buf)
                                      (not (get-buffer-window buf)))))
                      (-map (lambda (buf)
                              (cons buf
                                    (buffer-local-value 'buffer-display-time buf))))
                      (-filter #'cdr)
                      (-sort (-on (-compose #'not #'time-less-p) #'cdr))
                      (car)
                      (car)))
        (if current-prefix-arg
            (pop-to-buffer buf)
          (switch-to-buffer buf))
      (user-error "No recent buffer"))))


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
;; avy-goto-word-1 was recommended in https://irreal.org/blog/?p=9130,
;; but avy-goto-char-2 looks better on cognitive load.
(general-def
  "C-'" #'avy-goto-char-2)

(defun akirak/avy-pre-action-function (operand operation res)
  (let ((start (caar res))
        (window (cdr res)))
    (with-current-buffer (window-buffer window)
      (save-excursion
        (goto-char start)
        (cl-ecase operand
          (symbol (let ((begin (if (looking-at (rx symbol-start))
                                   (point)
                                 (re-search-backward (rx symbol-start) nil t)))
                        (end (save-excursion
                               (re-search-forward
                                (rx (group (+? anything)) symbol-end)
                                nil t))))
                    (funcall operation begin end))))))))

(cl-defmacro akirak/def-avy-edit-command (name
                                          operand operation
                                          &rest post-action)
  (declare (indent 1))
  `(defun ,(intern (concat "akirak/avy-" name)) ()
     (interactive)
     (let ((avy-all-windows t)
           (avy-pre-action (-partial #'akirak/avy-pre-action-function
                                     ,operand
                                     ,operation)))
       (save-excursion
         (save-window-excursion
           (call-interactively #'avy-goto-char-timer)))
       ,@post-action)))

;; Jump straight to the destination and do a thing
(general-def :prefix "C-;"
  "s" `(,(akirak/def-avy-edit-command "mirror-symbol"
           'symbol #'copy-region-as-kill)
        :wk "mirror symbol"))

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

(general-def :keymaps 'org-journal-mode-map :package 'org-journal
  [remap forward-page] #'org-journal-next-entry
  [remap backward-page] #'org-journal-previous-entry)

(general-def :keymaps 'Info-mode-map :package 'info
  "h" #'Info-up
  [remap forward-page] #'Info-next-preorder
  [remap backward-page] #'Info-prev)
;;;; Help and documentation
;;;;; Use <f1> as the prefix for help commands
(general-def
  [help ?.] #'helpful-at-point)

(general-def :package 'lsp-mode :keymaps 'lsp-mode-map
  [help ?.] #'lsp-describe-thing-at-point)

(akirak/bind-help
  "M" #'discover-my-major
  "xc" #'describe-char
  "xf" #'counsel-faces)

;; e.g. M-` M-m -> <f1> ESC m
(akirak/bind-help
  "ESC m" #'woman
  "ESC i" #'helm-info
  "ESC d" #'helm-dash)

;;;;; Dash Docs
(akirak/bind-user
  "d" '(nil :wk "doc")
  "da" #'dash-docs-activate-docset
  "dh" #'helm-dash
  "di" #'dash-docs-async-install-docset)

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

;;;;; Source navigation
;; Bind M-s M-s
(akirak/bind-search
  "M-s" #'xref-find-apropos)

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
  (defun akirak/compile-command ()
    (interactive)
    (require 'my/compile)
    (require 'my/helm/source/compile)
    (if (equal current-prefix-arg '(16))
        ;; If two prefixes are given, select the compilation buffer window.
        (if-let (buffer (get-buffer "*compilation*"))
            (if-let (window (get-buffer-window buffer))
                (select-window window)
              (pop-to-buffer buffer))
          (user-error "No compilation buffer"))
      (if-let ((plist (akirak/compile-detect-project)))
          (-let [(&plist :root :filename :command :helm-sources-fn) plist]
            (pcase current-prefix-arg
              ('(4)
               (find-file (expand-file-name filename root)))
              (_
               (let ((default-directory root))
                 (cond
                  (command
                   (funcall-interactively command))
                  (helm-sources-fn
                   (helm :prompt (format "%s project at [%s]: "
                                         filename
                                         (f-short root))
                         :sources (funcall helm-sources-fn))))))))
        ;; No root is detected.
        (akirak/helm-shell-command))))
  "C-x C"
  (defun akirak/helm-shell-command (&optional root)
    (interactive)
    (require 'my/helm/source/org)
    (require 'my/helm/action/org-marker)
    (let ((root (or root
                    (akirak/project-root default-directory)
                    default-directory)))
      (setq akirak/programming-recipe-mode-name "sh"
            akirak/helm-org-ql-buffers-files (org-multi-wiki-entry-files 'refs :as-buffers t))
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
     (let ((root (akirak/project-root default-directory)))
       (when ,other-window
         (or (other-window 1)
             (split-window-sensibly)))
       (let ((default-directory root))
         (call-interactively (quote ,command))))))

(cl-defmacro akirak/run-shell-command-silently-at-project-root (name command)
  `(defun ,name ()
     (interactive)
     (let ((default-directory (akirak/project-root default-directory)))
       (shell-command ,command))))

(cl-defmacro akirak/make-project-root-file-command (filename &key regexp name)
  `(defun ,(intern (format "akirak/open-%s-at-root" (or name (s-replace "." "-" filename)))) ()
     (interactive)
     (let* ((default-directory (akirak/project-root default-directory))
            (file (pcase (if ,regexp
                             (directory-files default-directory nil ,filename t)
                           (when (file-exists-p ,filename)
                             (list ,filename)))
                    (`(,file) file)
                    ('() (if (and (not regexp)
                                  (yes-or-no-p (format "%s does not exist. Create it?" ,filename)))
                             filename
                           (user-error "Aborted")))
                    (files (completing-read "File: " files)))))
       (find-file file))))

(akirak/bind-f8
  "A" #'treemacs-add-project-to-workspace
  "c" (akirak/run-at-project-root compile)
  "d" (defun akirak/project-dired ()
        (interactive)
        (dired (akirak/project-root default-directory)))
  "D" (akirak/run-at-project-root add-dir-local-variable)
  "e" (akirak/run-at-project-root ielm :other-window t)
  "g" #'deadgrep
  "n" '(:wk "nix")
  "nd" (akirak/make-project-root-file-command "default.nix")
  "ne" (akirak/run-shell-command-silently-at-project-root
        akirak/project-nix-shell-exit "nix-shell --run exit")
  "nf" (akirak/make-project-root-file-command "flake.nix")
  "nr" (akirak/run-at-project-root nix-repl :other-window t)
  "ns" (akirak/make-project-root-file-command "shell.nix")
  "r" (akirak/make-project-root-file-command "^README\\..+\\'" :regexp t :name "readme")
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
  )

;;;;; Git
(akirak/bind-admin
  "g" '(nil :wk "git")
  "gb" #'akirak/git-bookmark-repository
  "gc" #'akirak/git-clone-remote-repo
  "gl" #'magit-list-repositories
  "go" #'akirak/github-owned-repos
  "gr" #'commonplace-repos-counsel-rg
  "gs" #'akirak/github-starred-repos
  "gt" #'akirak/git-module-add-tags
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
  "nf" '(nil :wk "fetch")
  "nff" #'akirak/nix-prefetch-url)

;;;;; Org
(akirak/bind-admin
  "o" '(nil :wk "org")
  "oj" '(nil :wk "org-journal")
  ;; Add keybindings for switching the journal context
  "ojd" #'org-journal-new-date-entry)

;;;;; Remote connections (TRAMP)
(akirak/bind-admin
  "r" '(nil :wk "remote")
  "rk" #'helm-delete-tramp-connection)

;;;; Others
(general-def
  "C-c z"
  ;; https://www.reddit.com/r/emacs/comments/idz35e/emacs_27_can_take_svg_screenshots_of_itself/
  (defun screenshot-svg ()
    "Save a screenshot of the current frame as an SVG image.
Saves to a temp file and puts the filename in the kill ring."
    (interactive)
    (let* ((filename (make-temp-file "Emacs" nil ".svg"))
           (data (x-export-frames nil 'svg)))
      (with-temp-file filename
        (insert data))
      (kill-new filename)
      (message filename))))
