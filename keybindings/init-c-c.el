(general-def
  "C-c a" 'aya-create
  "C-c b" 'helm-bm
  "C-c c" #'org-capture
  "C-c e" 'aya-expand
  "C-c f" #'org-starter-alternative-find-file-by-key
  "C-c h" #'helm-corefighter
  "C-c i" 'scratch
  "C-c k" 'kill-compilation
  "C-c l" 'org-store-link
  "C-c m" 'bm-toggle
  "C-c n" #'counsel-org-capture-string
  "C-c p" 'yankpad-hydra/body
  "C-c r" #'rename-buffer
  "C-c s" 'symbol-overlay-put
  "C-c y" 'ivy-yasnippet)

(general-def origami-mode-map
  ;; "C-c o" #'origami-recursively-toggle-node
  "C-c o" #'origami-show-node)

(provide 'init-c-c)
