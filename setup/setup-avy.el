(use-package avy
  :general
  (:prefix "M-g"
           "w" #'avy-goto-char-2
           "l" #'avy-goto-line
           "o" #'avy-org-goto-heading-timer)
  :config/el-patch
  (el-patch-defun avy-forward-item ()
    (el-patch-swap (if (eq avy-command 'avy-goto-line)
                       (end-of-line)
                     (forward-sexp))
                   (cond
                    ((and (eq avy-command 'avy-goto-line)
                          (not lisp-mode))
                     (end-of-line))
                    ((bound-and-true-p smartparens-mode)
                     (sp-kill-hybrid-sexp nil))
                    (t (forward-sexp))))
    (point))
  :custom
  (avy-style 'at)
  (avy-styles-alist '((ivy-avy . pre)
                      (avy-goto-char-timer . at)
                      (akirak/avy-goto-symbol-in-window . pre)))
  (avy-keys (string-to-list "asdfghjkl")))

(use-package akirak/avy-extra
  :straight nil
  :load-path "extras")

(provide 'setup-avy)
