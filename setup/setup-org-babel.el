(use-package ob
  :after org
  :straight nil
  :config
  (defun akirak/org-babel-do-load-languages ()
    (org-babel-do-load-languages 'org-babel-load-languages
                                 org-babel-load-languages))
  (setq-default org-babel-load-languages
                '((emacs-lisp . t)
                  (shell . t)
                  (sqlite . t)
                  (graphql . t)
                  (mermaid . t)
                  (erd . t)
                  (translate . t)
                  (restclient . t)))
  :hook
  (emacs-startup . akirak/org-babel-do-load-languages)
  :custom
  (org-confirm-babel-evaluate nil))

(use-package ob-async)

;;;; Language supports

;;;;; Contributed packages shipped with the main org package

(use-package ob-shell
  :straight (:type built-in))

(use-package ob-emacs-lisp
  :straight (:type built-in))

(use-package ob-sqlite
  :straight (:type built-in))

;;;;; Third-party packages

(use-package ob-mermaid
  :after ob)

(use-package ob-restclient
  :after ob)

(use-package ob-erd
  :straight (:host github :repo "akirak/ob-erd")
  :after ob)

(use-package ob-graphql
  :after ob)

(use-package ob-translate
  :after ob)

(provide 'setup-org-babel)
