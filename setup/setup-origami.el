(use-package origami
  :config
  (akirak/bind-user :keymaps 'origami-mode-map "z" #'origami-toggle-node)
  (akirak/bind-fold :keymaps 'origami-mode-map
    ;; Particularly useful bold commands
    "SPC" #'origami-recursively-toggle-node
    "o" #'origami-show-only-noode
    "i" #'origami-redo
    "u" #'origami-undo
    ;; Other commands I may or may not use
    "f" #'origami-forward-fold-same-level
    "b" #'origami-backward-fold-same-level
    "n" #'origami-next-fold
    "p" #'origami-previous-fold
    "q" #'origami-reset))

(use-package lsp-origami
  :hook
  (lsp-mode . lsp-origami-mode))

(provide 'setup-origami)
