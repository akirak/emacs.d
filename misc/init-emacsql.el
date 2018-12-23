(use-package emacsql-sqlite
  :disabled t
  :init
  (let ((default-directory "~/.emacs.d/straight/repos/emacsql"))
    (if (file-exists-p "emacsql-sqlite.elc")
        (message "emacsql-sqlite has been already built")
      (compile "make sqlite/emacsql-sqlite")
      (compile "make emacsql-sqlite.elc"))))

(provide 'init-emacsql)
