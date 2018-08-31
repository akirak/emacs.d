(use-package hledger-mode
  :mode ("\\.ledger\\'" . hledger-mode)
  :config
  (add-hook 'company-backends 'hledger-company t))

(use-package hledger-input
  :straight hledger-mode
  :commands (hledger-capture))

(provide 'init-hledger)
