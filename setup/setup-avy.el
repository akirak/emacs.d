(use-package avy
  :config
  (defun akirak/avy-action-org-goto-heading (pt)
    (avy-action-goto pt)
    (org-back-to-heading))

  (defun akirak/avy-action-org-goto-heading (pt)
    (avy-action-goto pt)
    (org-back-to-heading)
    (let ((element (org-element-headline-parser
                    (save-excursion (org-end-of-subtree)))))
      (goto-char (plist-get (cadr element) :contents-begin))
      (when (org-at-property-block-p)
        (goto-char (cdr (org-get-property-block)))
        (end-of-line)
        (re-search-forward (rx bol bow) nil t))))

  (defun akirak/avy-action-org-store-link (pt)
    (avy-action-goto pt)
    (org-store-link nil 'interactive))

  ;; (add-to-list 'avy-dispatch-alist
  ;;              `(?K . akirak/avy-action-kill-line)
  ;;              t)
  (add-to-list 'avy-dispatch-alist
               '(?l . akirak/avy-action-org-store-link)
               t)

  (akirak/bind-jump
    "h"
    (defun akirak/avy-org-heading (arg &optional action)
      (declare (indent 1))
      (interactive "P")
      (let ((avy-all-windows (when arg t))
            (avy-action (or action #'akirak/avy-action-org-goto-heading)))
        (avy-with avy-goto-line
          (avy-jump (rx bol (+ "*") space)))))
    "M-h"
    (defun akirak/avy-org-heading-all-windows ()
      (interactive)
      (akirak/avy-org-heading '(4))))

  (general-def :prefix "C-c C-x" :package 'org :keymaps 'org-mode-map
    "l" '(nil :wk "insert link")
    "lh"
    (defun akirak/org-insert-link-to-avy-org-heading ()
      (interactive)
      (save-selected-window
        (akirak/avy-org-heading t #'akirak/avy-action-org-store-link))
      (org-insert-last-stored-link 1)))

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
   "C-\"" #'avy-goto-char-timer)
  :custom
  (avy-style 'at)
  (avy-styles-alist '((ivy-avy . pre)
                      (avy-goto-char-timer . at)
                      (akirak/avy-goto-symbol-in-window . pre)))
  (avy-keys (string-to-list "asdfghjk")))

(defun akirak/avy-action-kill-line (pt)
  (goto-char pt)
  (cond
   ((bound-and-true-p smartparens-mode)
    (sp-kill-hybrid-sexp nil))
   (t
    (kill-line)))
  (message "Killed: %s" (current-kill 0)))

(provide 'setup-avy)
