(defconst akirak/emacsql-directory
  (expand-file-name "contrib/emacsql" user-emacs-directory))

(add-to-list 'load-path akirak/emacsql-directory t)

(defvar akirak/emacsql-sqlite-activated
  (file-exists-p (expand-file-name "emacsql-sqlite.elc"
                                   akirak/emacsql-directory)))

(straight-use-package '(emacsql :type built-in))

(use-package emacsql-sqlite
  :straight emacsql
  :if akirak/emacsql-sqlite-activated)

(provide 'setup-emacsql-sqlite)
