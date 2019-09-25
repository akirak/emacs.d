(use-package polymode)

(use-package poly-markdown)

;; poly-org is a questionable package, because it seems to break
;; indentation in existing files.
(use-package poly-org
  ;; Disable poly-org
  :disabled t)

(use-package poly-vue
  :disabled t
  :straight (poly-vue :host github :repo "akirak/poly-vue"))

(provide 'setup-polymode)
