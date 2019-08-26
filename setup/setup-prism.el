(use-package prism
  :straight (prism :host github :repo "alphapapa/prism.el")
  :hook
  ((json-mode) . prism-mode))

(provide 'setup-prism)
