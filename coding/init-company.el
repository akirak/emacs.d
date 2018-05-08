(use-package company
  :diminish company-mode
  :init
  (add-hook 'after-init-hook 'global-company-mode)
  :general
  (:keymaps 'company-active-map :package 'company
            "C-n" #'company-select-next
            "C-p" #'company-select-previous))

(use-package company-quickhelp
  :after company
  :init
  (company-quickhelp-mode 1))

(use-package company-statistics
  :disabled t
  :after company
  :init
  (company-statistics-mode 1))

(provide 'init-company)
