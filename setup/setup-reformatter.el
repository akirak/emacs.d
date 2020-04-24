(use-package reformatter
  :config

  (reformatter-define nixfmt
    :program "nixfmt"
    :mode t)

  (reformatter-define prettier
    :program "prettier"
    :args (list "--stdin-filepath" (buffer-file-name))))

(use-package purty
  :after purescript-mode
  :straight (:host gitlab :repo "joneshf/purty"))

(provide 'setup-reformatter)
