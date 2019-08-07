(use-package pdf-tools
  :straight nil
  :preface
  (autoload 'pdf-occur-global-minor-mode "pdf-occur")
  :config
  (pdf-tools-install))

(provide 'setup-pdf-tools)
