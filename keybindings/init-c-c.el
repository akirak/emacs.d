(general-def
  "C-c a" 'aya-create
  "C-c b" 'helm-bm
  "C-c c" #'org-capture
  "C-c e" 'aya-expand
  "C-c h" #'helm-corefighter
  "C-c i" 'scratch
  "C-c k" 'kill-compilation
  "C-c l" 'org-store-link
  "C-c m" 'bm-toggle
  "C-c n" #'counsel-org-capture-string
  "C-c p" 'yankpad-hydra/body
  "C-c s" 'symbol-overlay-put
  "C-c y" 'ivy-yasnippet)

(provide 'init-c-c)
