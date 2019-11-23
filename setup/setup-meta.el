(defvar akirak/feature-list
  '(setup-shell-bindings ; Make ~C-a~, ~C-w~,  ~C-h~, etc. behave like in shells
    setup-key-translation ; Translate certain key combinations for ergonomics
    setup-exec-path
    setup-wrap
    setup-counsel    ; Basic Counsel commands
    setup-projectile ; Manage projects
    setup-swiper     ; Incremental search through the buffer using Ivy
    setup-aggressive-indent
    setup-lispy                         ; Efficient lisp editing
    setup-large-files
    setup-git-identity
    setup-ivy-frame-actions        ; Add frame-creation actions to Ivy
    setup-magit                    ; The Git porcelain for Emacs
    setup-vdiff
    setup-helpful         ; Extended helpful commands
    setup-company         ; Auto completion
    setup-eldoc           ; Display short help while coding
    setup-dired           ; File browser
    setup-rename          ; Utilities for rename operations
    setup-crux            ; Collection of utilities with modifications
    setup-helm-descbinds  ; Better keybinding search
    setup-helm-make       ; Choose a make command via Helm
    setup-anzu            ; Provide info in search & replace
    setup-smartparens     ; Parenthesis editing
    setup-org-make-toc    ; Generating TOCs
    setup-unpackaged      ; A bunch of useful commands
    setup-locate          ; Configure locate and updatedb
    setup-org-custom-commands
    setup-ace-window      ; An alternative way for window manipulation
    setup-outshine                 ; Extra support for outline editing
    setup-counsel-org-capture-string
    setup-org-refile                    ; Enhance org-refile
    setup-org-download                  ; Screenshots
    setup-counsel-org-clock
    setup-link-hint                     ; Jumping to a URL
    setup-helm-org-rifle
    setup-ibuffer
    setup-prescient
    setup-lsp                          ; Language-agnostic IDE toolkit
    setup-rich-minority                ; Whitelist minor modes
    setup-clipboard                    ; Clipboard integration
    setup-ivy-omni-org
    setup-window-management
    setup-system-tools
    setup-flycheck
    setup-navigation-bindings
    setup-spell
    setup-git-clone
    setup-clipurl
    setup-org-offtime
    setup-multiple-cursors
    setup-treemacs
    setup-screencast
    setup-pass
    setup-alert))

(mapc #'akirak/require akirak/feature-list)

(provide 'setup-meta)
