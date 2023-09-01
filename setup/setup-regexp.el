(use-package anzu
  :config
  (global-anzu-mode 1)
  :bind
  ([remap query-replace] . anzu-query-replace)
  ([remap query-replace-regexp] . anzu-query-replace-regexp))

(use-package re-builder
  :straight (:type built-in)
  :custom
  (reb-re-syntax 'rx)

  :config/el-patch
  (defun reb-empty-regexp ()
    (cond ((reb-lisp-syntax-p) (el-patch-swap "'()" "`(and bol)"))
          (t "")))
  :config
  ;; TODO: Add a replace command which is based on
  ;; https://karthinks.com/software/bridging-islands-in-emacs-1/
  
  (akirak/bind-mode :keymaps 'reb-mode-map
    "b" #'reb-change-target-buffer
    "c" #'reb-change-syntax
    "q" #'reb-quit
    "w" #'reb-copy)
  (akirak/bind-jump :keymaps 'reb-mode-map
    "M-n" #'reb-next-match
    "M-p" #'reb-prev-match
    "n" #'reb-next-match
    "p" #'reb-prev-match)

  (akirak/bind-search
    "r" #'re-builder))

(provide 'setup-regexp)
