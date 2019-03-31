(use-package restclient)

(use-package company-restclient
  :after restclient
  :commands company-restclient
  :hook
  (restclient-mode
   . (lambda ()
       (setq-local company-backends '(company-restclient company-capf)))))

(provide 'setup-restclient)
