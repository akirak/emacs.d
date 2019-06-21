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
          (restclient . t))))

(use-package ob-restclient)

(provide 'setup-org-babel)
