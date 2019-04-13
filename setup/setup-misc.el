;; The following packages are enhancements to Emacs that don't require
;; much configuration

;; Install smex for sorting M-x candidates
(use-package smex)
;; Save minibuffer history
(use-package savehist)
;; Install system packages
(use-package helm-system-packages :after helm 
  :commands (helm-system-packages))
;; Run (root) systemd operations
(use-package helm-systemd :after helm
  :commands (helm-systemd))
(use-package hl-todo
  :hook (prog-mode . hl-todo-mode))
(use-package fwb-cmds
  :straight (fwb-cmds :host github :repo "tarsius/fwb-cmds"))
(use-package helm-tail :after helm
  :straight (helm-tail :host github :repo "akirak/helm-tail")
  :commands (helm-tail))
(use-package counsel-projectile
  :after (projectile counsel)
  :init
  (counsel-projectile-mode 1))
;; Distraction-free editing
(use-package olivetti
  :disabled t
  :commands (turn-on-olivetti-mode)
  :custom (olivetti-body-width 92))
(use-package fontify-face
  :hook
  (emacs-lisp . (lambda () (fontify-face-mode 1))))
(use-package rainbow-mode
  ;; :diminish 'rainbow-mode
  :commands (rainbow-mode)
  :hook
  (prog-mode . (lambda () (rainbow-mode 1))))
;; Manage docker services
(use-package docker)                    
;; Manage daemons
(use-package prodigy)
(use-package helm-linux-disks
  :straight (helm-linux-disks :host github
                              :repo "akirak/helm-linux-disks")
  :commands (helm-linux-disks)
  :custom
  (linux-disk-terminal-type 'akirak/shell-new))
;; Profile the startup process
(use-package esup
  :commands (esup))
(use-package git-modes)
(use-package fix-word
  :commands (fix-word-upcase fix-word-downcase fix-word-capitalize)
  :hook
  (prog-mode . (lambda () (setq fix-word-thing 'symbol)))
  :general
  ([remap upcase-word] 'fix-word-upcase
   [remap downcase-word] 'fix-word-downcase
   [remap capitalize-word] 'fix-word-capitalize))
(use-package scratch
  :commands (scratch)
  :config
  (add-to-list 'scratch-mode-alist '(helpful-mode . emacs-lisp-mode))
  (add-to-list 'scratch-mode-alist '(help-mode . emacs-lisp-mode)))
(use-package dumb-jump
  ;; Don't enable dumb-jump-mode. Bind only necessary commands. 
  :custom
  (dumb-jump-selector 'ivy))
(use-package iedit)
(use-package comment-dwim-2
  :general
  ("M-;" 'comment-dwim-2))
(use-package narrow-or-widen
  :straight nil
  :load-path "contrib/misc")
;; Edit Org-Mode lists like in word processors
(use-package org-autolist 
  :after org
  ;; :diminish 'org-autolist-mode
  :init
  (add-hook 'org-mode-hook #'org-autolist-mode))
;; Allow you to bookmark headings in Org-Mode
(use-package org-bookmark-heading
  :after org)
(use-package org-bullets :after org
  :init
  (add-hook 'org-mode-hook 'org-bullets-mode))
;; Exporting mind maps
(use-package org-mind-map
  :after (org ox))
(use-package easy-kill
  :general
  ([remap kill-ring-save] #'easy-kill
   "M-m" #'easy-mark))
(use-package symbol-overlay
  :commands (symbol-overlay-put symbol-overlay-mode))
(use-package helm-dash :after helm
  :custom
  (helm-dash-browser-func #'akirak/display-url-for-referencing))
(use-package plumber
  :straight (plumber :host github :repo "akirak/plumber.el")
  :config
  (require 'plumber-config)
  (plumber-global-mode 1)
  :custom
  (plumber-avy-word-style 'pre)
  (plumber-enable-keybindings t))
(use-package undo-propose
  :config
  (undo-propose-mode)
  :general
  ("C-x u" 'undo-propose))
(use-package akirak/org-refile
  :straight nil)
(use-package default-text-scale
  :commands (default-text-scale-increase default-text-scale-decrease)
  :custom
  (default-text-scale-amount 3))
(use-package focus)
(use-package beginend
  :config
  (beginend-global-mode 1))
(use-package executable
  :straight nil
  :hook
  (after-save . executable-make-buffer-file-executable-if-script-p))
(use-package buffer-move
  :commands (buf-move-up buf-move-down buf-move-left buf-move-right))
(use-package browse-at-remote
  :commands (browse-at-remote))
(use-package git-attr-linguist
  :straight git-attr
  :commands (git-attr-linguist)
  :hook (find-file . git-attr-linguist))
(use-package page-break-lines
  :hook ((doc-mode
          emacs-lisp-mode
          compilation-mode
          outline-mode
          prog-mode
          haskell-mode
          help-mode
          magit-mode) . page-break-lines-mode))
(defun akirak/shrink-whitespace ()
  (interactive)
  (cond
   ((and (integerp current-prefix-arg)
         (>= current-prefix-arg 0))
    (if (looking-at (rx (* space) eol))
        (progn
          (end-of-line)
          (insert (make-string (max 0 (- current-prefix-arg
                                         (car (posn-col-row (posn-at-point)))))
                               32)))
      (delete-horizontal-space)
      (insert (make-string current-prefix-arg 32))))
   ((and (not current-prefix-arg)
         (looking-at (rx (* space) eol)))
    (delete-horizontal-space))                
   (t (call-interactively 'cycle-spacing))))
(general-def [remap delete-horizontal-space] 'akirak/shrink-whitespace)
(use-package string-inflection
  :general
  (:keymaps 'akirak/generic-prefix-map
            "." (defrepeater 'string-inflection-cycle)
            "c" #'akirak/string-inflection-hydra/body)
  :config
  (akirak/bind-generic :keymaps 'java-mode-map
    "." (defrepeater 'string-inflection-java-style-cycle))
  (akirak/bind-generic :keymaps 'python-mode-map
    "." (defrepeater 'string-inflection-python-style-cycle))
  (defhydra akirak/string-inflection-hydra (:hint nil)
    "
string inflection
[_C_] CamelCase        [_-_] lisp-case
[_c_] lowerCamelcase   [_\__] under_score
[_u_] UPCASE           [_=_] Capital_Underscore
"
    ("_" string-inflection-underscore)
    ("C" string-inflection-camelcase)
    ("c" string-inflection-lower-camelcase)
    ("-" string-inflection-lisp)
    ("=" string-inflection-capital-underscore)
    ("u" string-inflection-upcase)
    ("SPC" string-inflection-all-cycle "all cycle")
    ("q" nil "quit" :exit t)))
(use-package comment-tags
  :config
  (akirak/bind-generic :keymaps 'comment-tags-mode-map
    "'" (defrepeater 'comment-tags-next-tag))
  :hook (prog-mode . comment-tags-mode)
  :custom
  (comment-tags-case-sensitive t)
  (comment-tags-comment-start-only t))
(use-package deadgrep
  :commands deadgrep
  :general
  ("C-x ?" 'deadgrep))
(use-package counsel-tramp
  :commands (counsel-tramp))
(use-package counsel-world-clock)
(use-package align
  :general
  (:keymaps 'akirak/align-prefix-map
            "a" 'align))
(use-package ansi-color
  :ensure nil
  :hook (compilation-filter . colorize-compilation-buffer)
  :preface
  (autoload 'ansi-color-apply-on-region "ansi-color")
  (defun colorize-compilation-buffer ()
    (let ((inhibit-read-only t))
      (ansi-color-apply-on-region (point-min) (point-max)))))

(provide 'setup-misc)
