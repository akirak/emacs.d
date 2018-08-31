;;; init-org-babel.el --- org-babel config -*- lexical-binding: t -*-

(setq-default org-confirm-babel-evaluate nil)

(with-eval-after-load 'ob
  (require 'ob-ditaa)
  (require 'ob-dot)
  (org-babel-do-load-languages
   'org-babel-load-languages
   '((emacs-lisp . t)
     (ditaa . t)
     (shell . t)
     (python . t)
     (sqlite . t)
     (hledger . t)
     (dot . t)))
  (add-to-list 'org-src-lang-modes '("dot" . graphviz-dot)))

(provide 'init-org-babel)
;;; init-org-babel.el ends here
