(defvar akirak/feature-list
  '(setup-shell-bindings ; Make ~C-a~, ~C-w~,  ~C-h~, etc. behave like in shells
    setup-key-translation ; Translate certain key combinations for ergonomics
    setup-exec-path
    setup-wrap
    setup-whitespace
    setup-counsel    ; Basic Counsel commands
    setup-projectile ; Manage projects
    setup-swiper     ; Incremental search through the buffer using Ivy
    setup-aggressive-indent
    setup-lispy                         ; Efficient lisp editing
    setup-git-identity
    setup-direnv
    setup-ivy-frame-actions        ; Add frame-creation actions to Ivy
    setup-magit                    ; The Git porcelain for Emacs
    setup-helpful                  ; Extended helpful commands
    setup-shell
    setup-company                  ; Auto completion
    setup-eldoc                    ; Display short help while coding
    setup-dired                    ; File browser
    setup-multi-term               ; Terminal emulator
    setup-aweshell                 ; Enhance eshell
    setup-devdocs
    setup-rename          ; Utilities for rename operations
    setup-crux            ; Collection of utilities with modifications
    setup-google-translate          ; Translate a word in a buffer
    ;; setup-japanese
    setup-helm-descbinds            ; Better keybinding search
    setup-helm-make                 ; Choose a make command via Helm
    setup-anzu                      ; Provide info in search & replace
    setup-smartparens               ; Parenthesis editing
    setup-org-make-toc              ; Generating TOCs
    setup-unpackaged                ; A bunch of useful commands
    setup-locate                    ; Configure locate and updatedb
    setup-org-custom-commands
    setup-minor-mode-hydra
    setup-ace-window      ; An alternative way for window manipulation
    setup-yasnippet       ; Snippets for programming
    setup-yankpad
    setup-sidebar                  ; ibuffer and dired sidebars
    setup-outshine                 ; Extra support for outline editing
    setup-init-time-log            ; Log init time
    setup-autoinsert               ; File templates
    setup-counsel-org-capture-string
    setup-org-refile                    ; Enhance org-refile
    setup-org-download                  ; Screenshots
    setup-counsel-org-clock
    setup-org-mind-map
    setup-link-hint                     ; Jumping to a URL
    setup-helm-org-rifle
    setup-ibuffer
    setup-smart-jump                ; Jump to a definition / reference
    setup-lsp                       ; Language-agnostic IDE toolkit
    setup-rich-minority             ; Whitelist minor modes
    setup-clipboard                 ; Clipboard integration
    setup-ivy-omni-org
    setup-bm
    setup-window-management
    setup-system-tools
    setup-flycheck
    setup-navigation-bindings
    setup-spell
    setup-git-clone
    setup-clipurl
    setup-idle
    setup-multiple-cursors
    setup-treemacs
    setup-perfect-margin
    setup-screencast
    setup-string-inflection
    setup-pass
    setup-epub
    setup-git-auto-commit
    setup-alert
    setup-slack))

(mapc #'akirak/require akirak/feature-list)

(provide 'setup-meta)
