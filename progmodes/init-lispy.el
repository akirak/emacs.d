(straight-override-recipe '(lispy :host github
                                  :repo "akirak/lispy" :branch "master"
                                  :upstream (:host github
                                                   :repo "abo-abo/lispy")))

(use-package lispy
  :init
  (add-hook 'emacs-lisp-mode-hook (lambda () (lispy-mode 1)))
  (add-hook 'minibuffer-setup-hook (lambda ()
                                     (when (eq this-command 'eval-expression)
                                       (lispy-mode 1))))
  :general
  ;; Bind M-m to easy-mark (from easy-kill package) instead
  (:keymaps 'lispy-mode-map "M-m" nil))

(provide 'init-lispy)
