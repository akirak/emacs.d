(use-package avy
  :config
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
    "s"
    (defun akirak/avy-symbol-overlay ()
      (interactive)
      (let ((keywords (-map #'car symbol-overlay-keywords-alist)))
        (when keywords
          (avy-with akirak/avy-symbol-overlay
            (avy-jump (rx-to-string `(and symbol-start
                                          (or ,@keywords)))
                      :action
                      (lambda (point)
                        (goto-char point)
                        (re-search-backward (rx symbol-start)))
                      :window-flip avy-all-windows
                      :pred
                      (lambda ()
                        (member (thing-at-point 'symbol t)
                                keywords)))))))

    "h" #'akirak/org-avy-heading
    "M-h"
    (defun akirak/avy-org-heading-all-windows ()
      (interactive)
      (akirak-org-avy-heading '(4))))

  (general-def :package 'org :keymaps 'org-mode-map  :prefix "C-c C-x"
    "lh"
    (defun akirak/org-insert-link-to-avy-org-heading ()
      (interactive)
      (save-selected-window
        (akirak-org-avy-heading t #'akirak/avy-action-org-store-link))
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
   "C-'" #'avy-goto-char-timer)
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

(use-package eat
  :straight (:host github :repo "akirak/eat")
  :commands (eat-edit)
  :general
  ("C-;" #'eat-edit))

(provide 'setup-avy)
