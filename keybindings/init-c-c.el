(general-def
  "C-c l" 'org-store-link
  "C-c p" 'yankpad-map
  "C-c s" 'symbol-overlay-put
  "C-c u" '(nil :wk "ui toggle")
  "C-c u s" 'symbol-overlay-remove-all
  "C-c v" '(nil :wk "display-buffer")
  "C-c v s" '((lambda () (interactive) (pop-to-buffer "*scratch*")) :wk "scratch")
  "C-c y" 'yankpad-insert)

(provide 'init-c-c)
