(general-def :prefix "C-."
  "a" #'embrace-add
  "c" #'embrace-change
  "d" #'embrace-delete)

(general-def
  "C-c a" 'aya-create
  "C-c b" 'helm-bm
  "C-c c" #'org-capture
  ;; For C-c d, see akirak/bind-mode-key
  "C-c e" 'aya-expand
  "C-c h" #'helm-corefighter
  "C-c i" 'scratch
  "C-c k" 'kill-compilation
  "C-c l" 'org-store-link
  "C-c m" 'bm-toggle
  "C-c n" #'counsel-org-capture-string
  "C-c p" 'yankpad-insert
  "C-c s" 'symbol-overlay-put
  "C-c u" #'winner-undo-repeat
  "C-c U" #'winner-redo-repeat
  "C-c y" 'ivy-yasnippet
  "C-c '" #'outorg-edit-as-org)

(general-def :keymaps 'org-mode-map :package 'org
  "C-c o" nil)

;; (general-def origami-mode-map
;;   ;; "C-c o" #'origami-recursively-toggle-node
;;   "C-c o" #'origami-show-node)

(provide 'init-c-c)
