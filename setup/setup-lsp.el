(use-package lsp-mode
  :commands lsp
  :hook
  (lsp-mode . (lambda () (flycheck-mode 1))))

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
  (lsp-ui-mode . lsp-ui-sideline-mode)
  (lsp-ui-mode . (lambda () (eldoc-mode (not lsp-ui-mode))))
  :custom
  (lsp-ui-doc-delay 0.3)
  (lsp-ui-doc-header t)
  (lsp-ui-doc-include-signature t)
  (lsp-ui-doc-position (quote at-point)))

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

;;;; Configuration for specific clients
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

(use-package lsp-haskell
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

(use-package lsp-vetur
  :straight lsp-mode
  :after vue-mode)

(provide 'setup-lsp)
