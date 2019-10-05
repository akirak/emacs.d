(use-package restclient
  :mode (("\\.rest\\'" . restclient-mode)
         ("\\.restclient\\'" . restclient-mode))
  :config
  (remove-hook 'restclient-mode-hook 'restclient-outline-mode)
  (setcdr (assoc "application/json" restclient-content-type-modes #'string-equal)
          'json-mode))

(major-mode-hydra-define 'restclient-mode
  (:title "Restclient")
  ("Toggle"
   (("o" restclient-outline-mode :toggle t))))

(provide 'setup-restclient)
