(general-def
  "C-c b" 'helm-bm
  "C-c i" 'scratch
  "C-c l" 'org-store-link
  "C-c m" 'bm-toggle
  "C-c p" 'yankpad-map
  "C-c s" 'symbol-overlay-put
  "C-c u" '(nil :wk "ui toggle")
  "C-c u s" 'symbol-overlay-remove-all
  "C-c y" 'yankpad-insert)

(provide 'init-c-c)
