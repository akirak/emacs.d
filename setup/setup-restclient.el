(use-package restclient
  :mode (("\\.rest\\'" . restclient-mode)
         ("\\.restclient\\'" . restclient-mode)))

(use-package company-restclient
  :company restclient-mode)

(provide 'setup-restclient)
