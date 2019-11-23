(use-package avy
  :general
  (:prefix "M-g"
           "l" #'avy-goto-line
           "o" #'avy-org-goto-heading-timer)
  :config
  (add-to-list 'avy-dispatch-alist
               `(?K . akirak/avy-action-kill-line)
               t)
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

(defun akirak/avy-action-kill-line (pt)
  (goto-char pt)
  (cond
   ((bound-and-true-p smartparens-mode)
    (sp-kill-hybrid-sexp nil))
   (t
    (kill-line)))
  (message "Killed: %s" (current-kill 0)))

(use-package akirak/avy-extra
  :straight nil
  :load-path "extras"
  :general
  ("M-z" #'akirak/M-z))

(provide 'setup-avy)
