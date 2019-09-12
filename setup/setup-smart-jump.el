(use-package smart-jump
  :config
  (add-to-list 'smart-jump-default-mode-list '(elisp-mode lisp-interaction-mode))
  (smart-jump-setup-default-registers)
  :custom
  (smart-jump-jump-key "M-g .")
  (smart-jump-pop-key "M-g ,")

  ;; I don't like opening a new frame to display a function
  ;; definition. I am not sure if these features should be provided by
  ;; this package. lsp-ui provides a better user interface.

  ;; (smart-jump-peek-key "C-1")
  ;; (smart-jump-refs-key "C-2")

  ;; Override if you don't want to use the default config in some
  ;; modes.

  ;; (smart-jump-default-mode-list )
  )

(provide 'setup-smart-jump)
