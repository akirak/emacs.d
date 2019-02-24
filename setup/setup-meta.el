(defvar akirak/feature-list
  '(setup-shell-bindings ; Make ~C-a~, ~C-w~,  ~C-h~, etc. behave like in shells
    setup-key-translation ; Translate certain key combinations for ergonomics
    setup-counsel         ; Basic Counsel commands
    setup-projectile      ; Manage projects
    setup-swiper     ; Incremental search through the buffer using Ivy
    setup-aggressive-indent
    setup-lispy        ; Efficient lisp editing
    setup-ivy-filthy-rich ; Provide more information via Ivy/Counsel commands
    setup-ivy-frame-actions        ; Add frame-creation actions to Ivy
    setup-magit                    ; The Git porcelain for Emacs
    setup-helpful                  ; Extended helpful commands
    setup-company                  ; Auto completion
    setup-eldoc                    ; Display short help while coding
    setup-dired                    ; File browser
    setup-multi-term               ; Terminal emulator
    setup-aweshell                 ; Enhance eshell
    setup-rename                   ; Utilities for rename operations
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
    setup-ace-window      ; An alternative way for window manipulation
    setup-yasnippet       ; Snippets for programming
    setup-yankpad
    setup-sidebar                  ; ibuffer and dired sidebars
    setup-outshine                 ; Extra support for outline editing
    setup-init-time-log            ; Log init time
    setup-autoinsert               ; File templates
    setup-org-capture              ; Org-capture templates
    setup-org-starter              ; Org starter
    setup-counsel-org-capture-string
    setup-org-refile                    ; Enhance org-refile
    setup-org-download                  ; Screenshots
    setup-org-mind-map
    setup-link-hint                     ; Jumping to a URL
    setup-helm-org-rifle
    setup-ibuffer
    setup-smart-jump                ; Jump to a definition / reference
    setup-lsp                       ; Language-agnostic IDE toolkit
    setup-japanese
    setup-rich-minority                 ; Whitelist minor modes
    setup-clipboard                     ; Clipboard integration
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
    setup-hyperspace))

(mapc #'akirak/require akirak/feature-list)

(provide 'setup-meta)