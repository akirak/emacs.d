(use-package reformatter
  :config

  (reformatter-define nixfmt
    :program "nixfmt"
    :mode t)

  (reformatter-define prettier
    :program "prettier"
    :args (list "--stdin-filepath" (buffer-file-name)))

  (reformatter-define mix-format
    :program "mix"
    :mode t
    ;; In "mix format", if any of the files is -, then the output is
    ;; read from stdin and written to stdout.
    :args '("format" "-")))

(use-package purty
  :after purescript-mode
  :straight (:host gitlab :repo "joneshf/purty"))

(provide 'setup-reformatter)
