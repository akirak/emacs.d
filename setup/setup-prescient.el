(use-package prescient
  :config
  (prescient-persist-mode 1))

(use-package ivy-prescient
  ;; You must load counsel before ivy-prescient
  ;; See https://github.com/raxod502/prescient.el/tree/12ad508c447625918b4d0d93214a6f92f77d5dad#usage
  :after (ivy counsel)
  :config
  (ivy-prescient-mode t)
  ;; Disable ivy-prescient for the command.
  ;; See https://github.com/raxod502/prescient.el/issues/38
  (add-to-list 'ivy-sort-functions-alist '(ivy-omni-org . ivy-string<)))

(use-package company-prescient
  :after company
  :hook
  (company-mode . company-prescient-mode))

(provide 'setup-prescient)
