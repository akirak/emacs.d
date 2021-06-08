(use-package ob
  :after org
  :straight nil
  :config
  (defun akirak/org-babel-do-load-languages ()
    (org-babel-do-load-languages 'org-babel-load-languages
                                 org-babel-load-languages))
  (setq-default org-babel-load-languages
                '((emacs-lisp . t)
                  (java . t)
                  (shell . t)
                  (elixir . t)
                  (python . t)
                  (sqlite . t)
                  (go . t)
                  (graphql . t)
                  (mermaid . t)
                  (erd . t)
                  (dot . t)
                  (ditaa . t)
                  (restclient . t)
                  (typescript . t)))
  :hook
  (emacs-startup . akirak/org-babel-do-load-languages)
  :custom
  (org-confirm-babel-evaluate nil))

(use-package ob-async)

;;;; Language supports

;;;;; Contributed packages shipped with the main org package

(use-package ob-shell
  :straight (:type built-in))

(use-package ob-python
  :straight (:type built-in))

(use-package ob-emacs-lisp
  :straight (:type built-in))

(use-package ob-sqlite
  :straight (:type built-in))

(use-package ob-dot
  :after ob
  :straight nil
  :init
  (add-to-list 'org-src-lang-modes '("dot" . graphviz-dot)))

(use-package ob-ditaa
  :after ob
  :straight nil)

;;;;; Third-party packages

(use-package ob-mermaid
  :after ob)

(use-package ob-restclient
  :after ob)

;; You'll need tsc and node in the path to use this package.
(use-package ob-typescript
  :after ob)

(use-package ob-elixir
  :straight (:host github :repo "akirak/ob-elixir" :branch "lexical-binding")
  :after ob)

(use-package ob-go
  :after ob)

(use-package ob-erd
  :straight (:host github :repo "akirak/ob-erd")
  :after ob)

(use-package ob-graphql
  :after ob)

(use-package ob-translate
  :after ob)

(provide 'setup-org-babel)
