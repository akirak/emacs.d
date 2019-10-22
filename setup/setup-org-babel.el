(use-package ob
  :after org
  :straight nil
  :init
  (defun akirak/org-babel-do-load-languages ()
    (org-babel-do-load-languages 'org-babel-load-languages
                                 org-babel-load-languages))
  :config
  (setq org-confirm-babel-evaluate nil)
  :hook
  (emacs-startup . akirak/org-babel-do-load-languages))

;; To add support for a language in org-babel, add the following configuration:

;; - Add =(LANG . t)= to =org-babel-load-languages= in =:init= section
;;   of a =use-package= directive. If necessary, add a custom mapping
;;
;; - to =org-src-lang-modes=.

(unless (bound-and-true-p org-babel-load-languages)
  (setq org-babel-load-languages
        '((emacs-lisp . t)
          (java . t)
          (shell . t)
          (python . t)
          (sqlite . t)
          (dot . t)
          (ditaa . t)
          (restclient . t)              ; requires ob-restclient
          )))

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

(use-package ob-restclient
  :after ob)

(provide 'setup-org-babel)
