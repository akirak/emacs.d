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
(use-package embrace
  ;; I probably don't need this package. Use sp-{rewrap,unwrap}-sexp
  ;; instead
  :disabled t)
(use-package dumb-jump
  ;; Don't enable dumb-jump-mode. Bind only necessary commands. 
  :custom
  (dumb-jump-selector 'ivy))
(use-package iedit)
(use-package comment-dwim-2
  :general
  ("M-;" 'comment-dwim-2))
(use-package narrow-or-widen
  :straight nil)
(use-package counsel-org-clock
  :custom
  (counsel-org-clock-goto-fallback-function 'org-agenda)
  (counsel-org-clock-default-action 'clock-in))
(use-package org-reverse-datetree
  :straight (org-reverse-datetree :host github
                                  :repo "akirak/org-reverse-datetree"))
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

(provide 'setup-misc)
