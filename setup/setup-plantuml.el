(use-package plantuml-mode
  :custom
  (plantuml-default-exec-mode 'executable)
  (plantuml-executable-path (executable-find "plantuml")))

(use-package flycheck-plantuml
  :after (flycheck plantuml-mode)
  :config
  (flycheck-plantuml-setup)
  :custom
  (flycheck-plantuml-executable (executable-find "plantuml")))

(provide 'setup-plantuml)
