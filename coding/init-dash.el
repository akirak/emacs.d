(use-package helm-dash :after helm
  :custom
  (helm-dash-browser-func #'akirak/display-url-for-referencing))

(provide 'init-dash)
