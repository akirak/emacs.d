(use-package lispy
  :init
  (add-hook 'emacs-lisp-mode-hook (lambda () (lispy-mode 1)))
  (add-hook 'minibuffer-setup-hook (lambda ()
                                     (when (eq this-command 'eval-expression)
                                       (lispy-mode 1))))
  :general
  (:keymaps 'lispy-mode-map
            ;; Bind M-m to easy-mark (from easy-kill package) instead
            "M-m" nil
            ;; Use outline-insert-heading rather than lispy-meta-return
            [remap lispy-outline-promote] 'outline-promote
            [remap lispy-outline-demote] 'outline-demote
            [remap lispy-meta-return] 'outline-insert-heading))

(provide 'init-lispy)
