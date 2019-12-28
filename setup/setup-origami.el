(use-package origami
  :config
  (akirak/bind-generic
    "o" (general-predicate-dispatch #'origami-mode
          (bound-and-true-p origami-mode) #'akirak/origami-hydra/body))
  (akirak/bind-generic :keymaps 'origami-mode-map
    "<tab>" (defrepeater #'origami-forward-toggle-node))
  (pretty-hydra-define akirak/origami-hydra
    (:title "origami" :quit-key ("C-g" "q"))
    ("Toggle"
     (("TAB" origami-forward-toggle-node "fwd toggle")
      ("SPC" origami-recursively-toggle-node "toggle rec.")
      ("o" origami-show-only-node "show")
      ("a" origami-toggle-all-nodes "toggle all"))
     "Navigation"
     (("f" origami-forward-fold "fwd")
      ("b" origami-backward-fold "bwd")
      ("j" origami-forward-fold-same-level "fwd same lv.")
      ("k" origami-backward-fold-same-level "bwd same lv.")
      ("n" origami-next-fold "next")
      ("p" origami-previous-foldp "prev"))
     "State management"
     (("U" origami-redo "redo")
      ("u" origami-undo "undo")
      ("C-q" origami-reset "reset")
      ("RET" origami-mode :toggle t)))))

(use-package lsp-origami
  :hook
  (lsp-mode . lsp-origami-mode))

(provide 'setup-origami)
