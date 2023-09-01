(use-package restclient
  :mode (("\\.rest\\'" . restclient-mode)
         ("\\.restclient\\'" . restclient-mode)
         ("\\.http\\'" . restclient-mode))
  :config
  (setcdr (assoc "application/json" restclient-content-type-modes #'string-equal)
          'json-mode))

(use-package restclient-helm
  :after restclient)

(major-mode-hydra-define 'restclient-mode
  (:title "Restclient")
  ("Toggle"
   (("o" restclient-outline-mode :toggle t))))

(provide 'setup-restclient)
