(use-package anzu
  :config
  (global-anzu-mode 1)
  :bind
  ([remap query-replace] . anzu-query-replace)
  ([remap query-replace-regexp] . anzu-query-replace-regexp))

(provide 'setup-anzu)
