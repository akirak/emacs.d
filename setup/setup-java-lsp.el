(use-package lsp-java
  :config
  (require 'dap-java))

(use-package javadoc-lookup
  :commands (javadoc-lookup)
  :general
  ("<f1>M-j" #'javadoc-lookup)
  :custom
  (javadoc-lookup-completing-read-function 'ivy-completing-read))

(provide 'setup-java-lsp)
