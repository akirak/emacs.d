(use-package lsp-mode
  :commands lsp)

(use-package company-lsp
  :after (lsp-mode company)
  :commands company-lsp
  :init
  (add-hook 'company-backends 'company-lsp))

(use-package lsp-ui :after lsp
  :commands lsp-ui-mode
  :config
  (add-hook 'lsp-ui-doc-mode-hook
            (lambda () (when lsp-ui-doc-mode (eldoc-mode -1))))
  (add-hook 'lsp-ui-mode-hook 'lsp-ui-doc-enable)
  :hook
  (lsp-mode . lsp-ui-mode))

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

(akirak/bind-generic :keymaps 'lsp-mode-map
  "l" '(hydra-lsp/body :wk "hydra-lsp"))

(provide 'setup-lsp)
