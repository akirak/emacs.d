(defconst akirak/lsp-java-directory
  (expand-file-name ".cache/lsp-java" user-emacs-directory))

(use-package lsp-java
  :init
  (unless (file-directory-p akirak/lsp-java-directory)
    (make-directory akirak/lsp-java-directory))
  (require 'dap-java)
  (require 'lsp-java)
  :hook
  (java-mode . lsp)
  :custom
  (lsp-java-server-install-dir akirak/lsp-java-directory))

(use-package javadoc-lookup
  :commands (javadoc-lookup)
  :general
  ("<f1>M-j" #'javadoc-lookup)
  :custom
  (javadoc-lookup-completing-read-function 'ivy-completing-read))

(use-package lsp-java-treemacs
  :straight lsp-java
  :after treemacs)

(provide 'setup-java-lsp)
