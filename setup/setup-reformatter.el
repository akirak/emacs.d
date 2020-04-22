(use-package reformatter
  :config
  (reformatter-define nixfmt
    :program "nixfmt"))

(use-package purty
  :after purescript-mode
  :straight (:host gitlab :repo "joneshf/purty"))

(provide 'setup-reformatter)
