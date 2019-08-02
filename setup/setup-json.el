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

(provide 'setup-json)