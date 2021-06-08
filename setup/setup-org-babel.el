(use-package ob
  :after org
  :straight nil
  :config
  (defun akirak/org-babel-do-load-languages ()
    (org-babel-do-load-languages 'org-babel-load-languages
                                 org-babel-load-languages))
  (setq-default org-babel-load-languages
                '((translate . t)
                  (mermaid . t)
                  (erd . t)
                  (shell . t)
                  (emacs-lisp . t)
                  (sqlite . t)
                  (restclient . t)
                  (graphql . t)))
  :hook
  (emacs-startup . akirak/org-babel-do-load-languages)
  :custom
  (org-confirm-babel-evaluate nil))

;;;; Natural languages

(use-package ob-translate
  :after ob)

;;;; Diagramming

(use-package ob-mermaid
  :after ob)

(use-package ob-erd
  :straight (:host github :repo "akirak/ob-erd")
  :after ob)

;;;; Everyday scripting

(use-package ob-shell
  :straight (:type built-in))

(use-package ob-emacs-lisp
  :straight (:type built-in))

(use-package ob-sqlite
  :straight (:type built-in))

;;;; Web

(use-package ob-restclient
  :after ob)

(use-package ob-graphql
  :after ob)

(provide 'setup-org-babel)
