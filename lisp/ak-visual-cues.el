(use-package highlight-indent-guides
  :init
  (add-hook 'prog-mode-hook 'highlight-indent-guides-mode))

(use-package rainbow-delimiters
  :init
  (add-hook 'prog-mode-hook #'rainbow-delimiters-mode))

(use-package fill-column-indicator
  :init
  (add-hook 'prog-mode-hook 'fci-mode))

(provide 'ak-visual-cues)
