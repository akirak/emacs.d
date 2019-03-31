(use-package web-mode
  :mode "\\.vue\\'")

(use-package company-web
  :hook
  (web-mode . (lambda ()
                (setq-local compan-backends
                            '(company-web-html
                              company-capf)))))

(provide 'setup-web-mode)
