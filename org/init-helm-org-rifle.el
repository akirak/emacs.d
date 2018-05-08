(require 'init-helm)

(use-package helm-org-rifle
  :after (helm org)
  :custom
  (helm-org-rifle-show-path t))

(provide 'init-helm-org-rifle)
