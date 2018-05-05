(use-package anzu
  :config
  (global-anzu-mode 1)
  :bind
  ([remap query-replace] . anzu-query-replace))

(provide 'init-anzu)
