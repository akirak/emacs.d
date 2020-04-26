(use-package avy
  :config
  (add-to-list 'avy-dispatch-alist
               `(?K . akirak/avy-action-kill-line)
               t)
  (akirak/bind-jump
    "h"
    (defun akirak/avy-org-heading (arg)
      (interactive "P")
      (let ((avy-all-windows (when arg t))
            ;; TODO: Make avy-action work
            (avy-action (lambda (pt)
                          (avy-action-goto pt)
                          (org-back-to-heading)
                          (let ((element (org-element-headline-parser
                                          (save-excursion (org-end-of-subtree)))))
                            (goto-char (plist-get (cadr element) :contents-begin))
                            (when (org-at-property-block-p)
                              (goto-char (cdr (org-get-property-block)))
                              (end-of-line)
                              (re-search-forward (rx bol bow) nil t))))))
        (avy-with avy-goto-line
          (avy-jump (rx bol (+ "*") space)))))
    "M-h"
    (defun akirak/avy-org-heading-all-windows ()
      (interactive)
      (akirak/avy-org-heading '(4))))
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
  :general
  ("M-z" #'avy-goto-char-in-line
   "C-'" #'avy-goto-char-timer)
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

(provide 'setup-avy)
