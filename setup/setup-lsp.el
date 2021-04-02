(use-package lsp-mode
  :commands (lsp lsp-deferred)
  :config

  (define-minor-mode lsp-format-on-save-mode
    "Format the buffer on save."
    nil " LSP format" nil
    (if lsp-format-on-save-mode
        (add-hook 'before-save-hook
                  #'lsp-format-buffer)
      (remove-hook 'before-save-hook
                   #'lsp-format-buffer)))

  (define-minor-mode lsp-organize-imports-on-save-mode
    "Format the buffer on save."
    nil " LSP imports" nil
    (if lsp-organize-imports-on-save-mode
        (add-hook 'before-save-hook
                  #'lsp-organize-imports)
      (remove-hook 'before-save-hook
                   #'lsp-organize-imports)))

  (add-hook 'lsp-mode-hook
            (defun lsp-disable-on-dsave-modes ()
              (unless lsp-mode
                (lsp-format-on-save-mode -1)
                (lsp-organize-imports-on-save-mode -1))))

  (defun akirak/setup-lsp ()
    (company-mode t)
    (eldoc-mode t)
    (flycheck-mode t)
    (lsp-enable-which-key-integration))
  ;; Update direnv to detect locally installed lsp servers
  ;; (advice-add 'lsp :before
  ;;             (lambda (&rest _args) (direnv-update-environment)))
  (defun akirak/javascript-lsp-deferred ()
    (unless (derived-mode-p 'json-mode)
      (lsp-deferred)))

  (general-add-hook 'lsp-file-watch-ignored
                    `(,(rx (any "/\\") ".direnv" eol))
                    'append)
  :hook
  (lsp-mode . akirak/setup-lsp)
  ((css-mode
    typescript-mode
    ;; Use psc-ide in purescript
    ;; purescript-mode
    ;; java-mode
    )
   . lsp-deferred)
  (javascript-mode . akirak/javascript-lsp-deferred)
  :custom
  (lsp-keymap-prefix "C-,")
  (lsp-enable-xref t)
  (lsp-enable-semantic-highlighting t)
  (lsp-prefer-capf t)
  (lsp-eldoc-render-all nil)
  ;; Since I have my own header line, I need only the symbol path.
  (lsp-headerline-breadcrumb-segments '(symbols))
  (lsp-headerline-breadcrumb-enable t)
  ;; Based on the performance guide
  ;; https://emacs-lsp.github.io/lsp-mode/page/performance/
  (lsp-completion-provider :capf)
  (lsp-idle-delay 0.5)
  (lsp-enable-file-watchers nil)
  (lsp-file-watch-threshold 100)
  (lsp-prefer-flymake nil)
  (lsp-log-io nil)
  (lsp-enable-folding nil)
  (lsp-enable-indentation t)
  (lsp-enable-imenu t)
  ;; (lsp-diagnostic-package :none)
  (lsp-enable-snippet nil)
  (lsp-enable-symbol-highlighting nil)
  (lsp-enable-links nil)
  (lsp-restart 'auto-restart))

(use-package lsp-ui
  :disabled t
  :commands lsp-ui-mode
  :hook
  (lsp-mode . lsp-ui-mode)
  (lsp-ui-mode . lsp-ui-sideline-mode)
  :custom
  (lsp-ui-doc-delay 0.8)
  (lsp-ui-doc-header nil)
  (lsp-ui-doc-include-signature t)
  (lsp-ui-doc-alignment 'window)
  (lsp-ui-doc-use-childframe t)
  (lsp-ui-doc-position (quote at-point)))

(use-package lsp-ivy
  :disabled t
  :config
  (akirak/bind-search :keymaps 'lsp-mode-map
    "M-i" #'lsp-ivy-workspace-symbol
    "S-M-i" #'lsp-ivy-global-workspace-symbol))

(use-package helm-lsp
  :after lsp-mode
  :general
  (:keymaps 'lsp-mode-map
            ;; Should be M-s M-s.
            [remap xref-find-apropos] #'helm-lsp-workspace-symbol))

(use-package lsp-treemacs
  :after lsp-mode)

;;;; Dap mode

(use-package dap-mode
  :after lsp-mode
  :config
  ;; Based on https://tychoish.com/post/emacs-and-lsp-mode/
  (add-hook 'dap-session-created
            (defun akirak/dap-hydra-on-create (&_rest)
              (dap-hydra)))
  (add-hook 'dap-terminated
            (defun akirak/dap-hydra-close (&_rest)
              (dap-hydra/nil)))

  (defun akirak/install-vscode-extension-if-missing (publisher name)
    (unless (f-directory-p (f-join dap-utils-extension-path "vscode"
                                   (concat publisher "." name)))
      (dap-utils-get-vscode-extension publisher name)))
  :general
  ("<S-f9>" #'dap-debug)
  (:keymaps 'lsp-command-map "d" #'dap-hydra)
  :hook
  (lsp-mode . dap-mode))

(use-package dap-chrome
  :straight dap-mode
  :after (dap-mode typescript-mode)
  :config
  (akirak/install-vscode-extension-if-missing "msjsdiag" "debugger-for-chrome"))

;;;; Additional LSP client packages which are not part of lsp-mode
(use-package lsp-eslint
  ;; You will need an executable of eslint to use this server, since
  ;; the server installation functionality of lsp-mode doesn't install
  ;; eslint itself.
  ;;
  ;; If you don't want to install eslint globally, you can add
  ;; nodePackages.eslint to shell.nix instead.
  :straight lsp-mode
  :after (lsp typescript-mode))

(use-package lsp-javascript
  :straight lsp-mode
  :after (lsp typescript-mode))

(use-package lsp-dockerfile
  :disabled t
  :after dockerfile-mode
  :hook
  (dockerfile-mode . lsp-dockerfile-enable)
  ;; :ensure-system-package
  ;; (docker-langserver . "sudo npm i -g dockerfile-language-server-nodejs")
  )

(use-package lsp-python
  :after python-mode
  :hook
  (python-mode . lsp-python-enable)
  ;; TODO: Install the executable using nix or something
  ;; :ensure-system-package python-language-server
  )

(use-package lsp-haskell
  :disabled t
  :after haskell-mode
  ;; To use lsp-haskell, you also need to install haskell-ide-server.
  ;; This can take a lot of time, so install it manually if you want
  ;; to write Haskell code.
  :straight (lsp-haskell :host github :repo "emacs-lsp/lsp-haskell")
  :config
  (defun akirak/maybe-turn-off-dante-mode ()
    (when (and (bound-and-true-p lsp-mode)
               (bound-and-true-p dante-mode)
               (derived-mode-p 'haskell-mode))
      (dante-mode -1)))
  :hook
  (lsp-mode . akirak/maybe-turn-off-dante-mode)
  :custom
  (lsp-haskell-process-path-hie "ghcide")
  (lsp-haskell-process-args-hie nil))

(use-package lsp-java
  :disabled t
  :config
  (require 'dap-java))

(use-package lsp-tailwindcss
  :after web-mode)

(provide 'setup-lsp)
