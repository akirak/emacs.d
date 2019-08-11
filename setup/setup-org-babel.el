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

(unless (bound-and-true-p org-babel-load-languages)
  (setq org-babel-load-languages
        '((emacs-lisp . t)
          (java . t)
          (shell . t)
          (python . t)
          (sqlite . t)
          (dot . t)
          (ditaa . t)
          (restclient . t) ; requires ob-restclient
          )))

;;;; Language supports

;;;;; Contributed packages shipped with the main org package

(use-package ob-dot
  :after ob
  :straight nil
  :init
  (require 'ox-org)
  (add-to-list 'org-src-lang-modes '("dot" . graphviz-dot)))

(use-package ob-ditaa
  :after ob
  :straight nil)

;;;;; Third-party packages

(use-package ob-restclient
  :after ob)

(provide 'setup-org-babel)
