(defvar akirak/feature-list
  '(setup-exec-path
    setup-wrap
    setup-counsel              ; Basic Counsel commands
    ;; setup-projectile ; Manage projects
    setup-swiper               ; Incremental search through the buffer using Ivy
    setup-aggressive-indent
    setup-lispy                         ; Efficient lisp editing
    setup-large-files
    setup-repos
    setup-tramp
    setup-magit                         ; The Git porcelain for Emacs
    setup-vdiff
    setup-helpful                   ; Extended helpful commands
    setup-company                   ; Auto completion
    setup-eldoc                     ; Display short help while coding
    setup-dired                     ; File browser
    setup-rename                    ; Utilities for rename operations
    setup-crux                      ; Collection of utilities with modifications
    setup-regexp
    setup-smartparens                   ; Parenthesis editing
    setup-tagedit
    setup-org-make-toc                  ; Generating TOCs
    setup-unpackaged                    ; A bunch of useful commands
    setup-org-custom-commands
    setup-ace-window                ; An alternative way for window manipulation
    ;; setup-outshine        ; Extra support for outline editing
    setup-org-refile                    ; Enhance org-refile
    setup-counsel-org-clock
    setup-link-hint                     ; Jumping to a URL
    setup-ibuffer
    ;; setup-prescient
    ;; setup-lsp      ; Language-agnostic IDE toolkit
    ;; setup-rich-minority                ; Whitelist minor modes
    setup-clipboard                     ; Clipboard integration
    setup-ivy-omni-org
    setup-window-management
    setup-flycheck
    setup-navigation-bindings
    ;; Use akirak-git-clone instead
    ;; setup-git-clone
    setup-clipurl
    ;; setup-org-offtime
    setup-multiple-cursors
    setup-treemacs
    setup-screencast
    ;; setup-alert
    setup-pass))

(mapc #'akirak/require akirak/feature-list)

(provide 'setup-meta)
