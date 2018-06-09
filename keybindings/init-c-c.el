(require 'init-fix-map)
(require 'init-ui-map)

(general-def
  "C-c b" 'helm-bm
  "C-c c" 'helm-org-capture-templates
  "C-c e" 'aya-expand
  "C-c f" '(:prefix-command akirak/fix-map :which-key "prefix")
  "C-c i" 'scratch
  "C-c l" 'org-store-link
  "C-c m" 'bm-toggle
  "C-c p" 'yankpad-hydra/body
  "C-c s" 'symbol-overlay-put
  "C-c u" '(akirak/ui-map :which-key "ui"))

(provide 'init-c-c)
