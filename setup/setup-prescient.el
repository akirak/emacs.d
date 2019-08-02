(use-package prescient
  :config
  (prescient-persist-mode 1))

(use-package ivy-prescient
  :after ivy
  :config
  (ivy-prescient-mode t))

(use-package company-prescient
  :after company
  :hook
  (company-mode . company-prescient-mode))

(provide 'setup-prescient)
