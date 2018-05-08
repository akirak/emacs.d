(straight-override-recipe '(lispy :host github
                                  :repo "akirak/lispy" :branch "outline"
                                  :files (:defaults "lispy-clojure.clj")
                                  :upstream (:host github
                                                   :repo "abo-abo/lispy")))

(use-package lispy
  :init
  (add-hook 'emacs-lisp-mode-hook (lambda () (lispy-mode 1)))
  (add-hook 'minibuffer-setup-hook (lambda ()
                                     (when (eq this-command 'eval-expression)
                                       (lispy-mode 1))))
  )

(provide 'init-lispy)
