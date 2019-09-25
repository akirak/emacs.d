(use-package lsp-mode
  :commands lsp)

(use-package lsp-clients
  :straight lsp-mode
  :hook ((web-mode
          vue-html-mode
          css-mode)
         . lsp-deferred))

(use-package lsp-server
  :straight (:host github :repo "akirak/lsp-server.el")
  :commands (lsp-server-install))

(use-package lsp-ui
  :commands lsp-ui-mode
  :hook
  (lsp-mode . lsp-ui-mode)
  (lsp-ui-mode . lsp-ui-doc-mode)
  (lsp-ui-mode . lsp-ui-peek-mode)
  (lsp-ui-mode . lsp-ui-sideline-mode))

(use-package lsp-treemacs
  :after lsp)

(use-package dap-mode
  :after lsp-mode
  :hook
  (lsp-mode . dap-mode)
  (dap-mode . dap-ui-mode))

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

;;;; Extra clients
(use-package lsp-dockerfile
  :straight (lsp-dockerfile :host github :repo "emacs-lsp/lsp-dockerfile")
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

(provide 'setup-lsp)
