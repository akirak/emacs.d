(use-package aggressive-indent
  :init
  (defun aggressive-indent-mode-on ()
    (interactive)
    (aggressive-indent-mode 1))
  :hook
  ((emacs-lisp-mode) . aggressive-indent-mode-on))

(provide 'init-aggressive-indent)
