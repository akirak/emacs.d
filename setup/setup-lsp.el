(use-package lsp-mode
  :commands lsp)

(use-package lsp-clients
  :straight lsp-mode
  :init
  (make-variable-buffer-local 'lsp-clients-typescript-server)
  (make-variable-buffer-local 'lsp-clients-javascript-typescript-server)
  :config
  (defun akirak/javascript-setup-lsp ()
    (if (executable-find "npm")
        (let* ((node-root (locate-dominating-file default-directory "node_modules"))
               (node-bin (expand-file-name "node_modules/.bin" node-root)))
          (when (file-executable-p (expand-file-name "javascript-typescript-stdio" node-bin))
            (setq-local lsp-clients-javascript-typescript-server
                        (expand-file-name "javascript-typescript-stdio" node-bin)))
          (when (file-executable-p (expand-file-name "typescript-language-server" node-bin))
            (setq-local lsp-clients-typescript-server
                        (expand-file-name "typescript-language-server" node-bin))))
      (user-error "npm is not found on the path"))
    (lsp))
  :hook (js-mode . akirak/javascript-setup-lsp))

(use-package lsp-ui
  :commands lsp-ui-mode
  :general
  ;; jump commands
  ;; (#'lsp-ui-peek-find-definitions
  ;;  #'lsp-ui-peek-find-references
  ;;  #'lsp-ui-peek-jump-forward
  ;;  #'lsp-ui-peek-jump-backward
  ;;  )
  ;; For browsing documentation, use lsp-ui-doc.
  :hook
  (lsp-mode . lsp-ui-mode))

(use-package dap-mode
  :after lsp-mode
  :config
  (dap-mode 1)
  (dap-ui-mode 1))

;; https://github.com/abo-abo/hydra/wiki/lsp-mode
(defhydra hydra-lsp (:exit t :hint nil)
  "
 Buffer^^               Server^^                   Symbol
-------------------------------------------------------------------------------------
 [_f_] format           [_M-r_] restart            [_d_] declaration  [_i_] implementation  [_o_] documentation
 [_m_] imenu            [_S_]   shutdown           [_D_] definition   [_t_] type            [_r_] rename
 [_x_] execute action   [_M-s_] describe session   [_R_] references   [_s_] signature"
  ("d" lsp-find-declaration)
  ("D" lsp-ui-peek-find-definitions)
  ("R" lsp-ui-peek-find-references)
  ("i" lsp-ui-peek-find-implementation)
  ("t" lsp-find-type-definition)
  ("s" lsp-signature-help)
  ("o" lsp-describe-thing-at-point)
  ("r" lsp-rename)

  ("f" lsp-format-buffer)
  ("m" lsp-ui-imenu)
  ("x" lsp-execute-code-action)

  ("M-s" lsp-describe-session)
  ("M-r" lsp-restart-workspace)
  ("S" lsp-shutdown-workspace))

;; lsp-treemacs provides project-wide error overview.
(use-package lsp-treemacs
  :after (treemacs lsp))

(akirak/bind-generic :keymaps 'lsp-mode-map
  "l" '(hydra-lsp/body :wk "hydra-lsp"))

(provide 'setup-lsp)
