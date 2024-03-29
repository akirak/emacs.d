;;;; General settings

(use-package company
  :general
  (:keymaps 'company-mode-map
            "M-/" #'company-complete)
  (:keymaps 'company-active-map :package 'company
            "C-f" #'company-complete-selection
            "C-n" #'company-select-next
            "C-p" #'company-select-previous
            "M-/" #'company-other-backend
            ;; Docstring is automatically displayed on idle
            ;; "<f1>" #'company-show-doc-buffer
            )
  :config
  (defun akirak/set-default-company-backends ()
    (set (make-local-variable 'company-backends)
         ;; TODO: Refine the default backend configuration
         (cond
          ((derived-mode-p 'css-mode)
           '(company-css
             (company-dabbrev-code
              company-gtags
              company-etags
              company-keywords)
             company-files
             company-dabbrev))
          ((derived-mode-p 'prog-mode)
           '(company-capf
             (company-dabbrev-code
              company-gtags
              company-etags
              company-keywords)
             company-files
             company-dabbrev))
          (t
           '(company-capf
             company-keywords
             company-dabbrev
             company-files)))))
  (cl-delete 'company-echo-metadata-frontend
             (default-value 'company-frontends))
  :hook
  ((prog-mode text-mode git-commit-mode) . company-mode)
  (company-mode . akirak/set-default-company-backends)
  :custom
  (company-idle-delay nil)
  (company-require-match nil)
  (company-auto-complete (lambda ()
                           (and (company-tooltip-visible-p)
                                (company-explicit-action-p))))
  (company-continue-commands '(not save-buffer
                                   save-some-buffers
                                   save-buffers-kill-terminal
                                   save-buffers-kill-emacs
                                   comint-previous-matching-input-from-input
                                   comint-next-matching-input-from-input
                                   completion-at-point))
  (company-dabbrev-other-buffers 'all)
  ;; Ignore Org buffers, buffers of encrypted files, and special buffers.
  ;; Based on https://www.reddit.com/r/emacs/comments/aqss7l/anyone_store_password_in_emacs/egjxi6e/
  (company-dabbrev-ignore-buffers (rx (or (and bos (any " *"))
                                          (and (or ".gpg" ".org") eos))))
  (company-dabbrev-downcase 'case-replace)
  (company-dabbrev-ignore-invisible nil)
  (company-dabbrev-code-other-buffers 'all)
  (company-tooltip-align-annotations nil))

(use-package conventional-commit
  :hook
  (git-commit-mode . conventional-commit-setup))

(use-package company-posframe
  :if (posframe-workable-p)
  :custom
  (company-tooltip-minimum-width 40)
  (company-posframe-show-indicator t)
  (company-posframe-show-metadata t)
  :hook
  (company-mode . company-posframe-mode))

;;;; Specific backends

;;;;; General
(use-package company-emoji
  :disabled t
  :after company)

(use-package company-dict
  :disabled t)

;;;;; Mode-specific backends

(use-package company-lsp
  :disabled t
  ;; For company completion and snippets
  ;;
  ;; If the language server for the language doesn't support returning
  ;; snippets, you can define one one your own by customizing the
  ;; variable `company-lsp--snippet-functions'.
  ;; See the following document for details.
  ;; https://github.com/tigersoldier/company-lsp#defining-completion-snippet-for-a-certain-language
  :after (company lsp)
  :company lsp-mode)

(use-package company-ansible
  :after (company ansible)
  :company ansible)

(use-package company-shell
  :after (company shell)
  :company shell-script-mode)

(use-package company-terraform
  :after (company hcl-mode)
  :company terraform-mode)

(use-package company-restclient
  :after (company restclient)
  :company restclient-mode)

(use-package nix-company
  :straight nix-mode
  :company (nix-mode . company-nix))

(use-package company-nixos-options
  :after (company nix-mode)
  :straight (:host github :repo "travisbhartwell/nix-emacs")
  :company nix-mode)

(use-package readline-complete
  :after (company comint)
  :company
  ((comint-mode shell-mode) . company-readline))

(use-package company-web-html
  :after (company)
  :straight company-web
  :functions (company-web-html)
  :company
  ((html-mode mhtml-mode) . company-web-html))

(use-package company-elixir
  :company elixir-mode)

(provide 'setup-company)
