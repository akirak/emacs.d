(use-package nim-mode)

(when-let ((bin (executable-find "nimsuggest")))
  (setq nimsuggest-path bin))

(provide 'init-nim)
