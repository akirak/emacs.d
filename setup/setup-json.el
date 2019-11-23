(use-package json-mode
  :mode (("\\.bowerrc$"     . json-mode)
         ("\\.jshintrc$"    . json-mode)
         ("\\.json_schema$" . json-mode)
         ("\\.json\\'" . json-mode))
  :bind (:package json-mode-map
                  :map json-mode-map
                  ("C-c <tab>" . json-mode-beautify))
  :config
  (make-local-variable 'js-indent-level))

(use-package counsel-jq
  :straight (counsel-jq :host github :repo "200ok-ch/counsel-jq")
  :config
  (akirak/bind-mode :package 'json-mode :keymaps 'json-mode-map
    "q" #'counsel-jq))

(provide 'setup-json)
