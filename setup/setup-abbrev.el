(use-package abbrev
  :straight (:type built-in)
  :init
  (setq abbrev-mode t)
  :config
  (akirak/bind-register
    "g" #'add-global-abbrev
    "l" #'edit-abbrevs)
  :custom
  ;; abbrev-file-name is set externally
  (save-abbrev 'silently))

(provide 'setup-abbrev)
