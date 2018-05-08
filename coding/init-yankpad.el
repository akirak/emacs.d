(require 'init-yasnippet)

(use-package yankpad
  :after (org yasnippet)
  :config
  (with-eval-after-load 'company
    (add-hook 'company-backends #'company-yankpad t)))

(provide 'init-yankpad)
